use normalize_path::NormalizePath;
use std::{fs::read_dir, path::PathBuf};

use cached::proc_macro::once;
use which::which;

#[derive(Clone, Debug)]
pub struct NimInstallationInfo {
    /// Path to the nim executable.
    pub path: PathBuf,
    /// Path to the nim packages directory.
    pub pkgs_path: PathBuf,
    /// Version of the nim compiler.
    pub version: String,
    /// List of paths to the standard library directories.
    pub std_lib_paths: Vec<PathBuf>,
}

fn run_command_and_get_output(command: Vec<&str>) -> (String, String) {
    let output = std::process::Command::new(command[0])
        .args(&command[1..])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    (stdout, stderr)
}

fn extract_version(header_message: &str) -> String {
    let first_dot = header_message.find(".").unwrap_or(0);
    let version_start = header_message[0..first_dot].rfind(" ");
    let Some(version_start) = version_start else {
        return "unknown".into();
    };
    let version_end = header_message[version_start + 1..].find(" ");
    let Some(mut version_end) = version_end else {
        return "unknown".into();
    };
    version_end += version_start + 1;

    header_message[version_start + 1..version_end].trim().into()
}

#[once]
pub fn get_nim_installation_info() -> NimInstallationInfo {
    let nim_path = which("nim");
    let Ok(nim_path) = nim_path else {
        panic!("nim not found in PATH. Check if Nim is installed.");
    };

    let (version_stdout, _) = run_command_and_get_output(vec!["nim", "--version"]);
    let (_, lib_paths_stderr) = run_command_and_get_output(vec!["nim", "dump"]);

    let header_message = version_stdout.split("\n").take(1).collect::<Vec<_>>()[0]; // Nim Compiler Version x.y.z [os: platform]
    let version = extract_version(header_message);
    let lib_paths = if !lib_paths_stderr.is_empty() {
        lib_paths_stderr.split("\n").skip(2).collect::<Vec<_>>()
    } else {
        Vec::new()
    };

    let lib_paths = lib_paths
        .iter()
        .map(|path| PathBuf::from(path.trim()))
        .filter(|path| path.exists())
        .collect::<Vec<_>>();

    NimInstallationInfo {
        path: nim_path.clone(),
        pkgs_path: nim_path.join("../../pkgs").normalize(),
        version,
        std_lib_paths: lib_paths,
    }
}

pub fn get_likely_lib_paths(lib_name: &str) -> Box<dyn Iterator<Item = PathBuf>> {
    let installation = get_nim_installation_info();
    let lib_name_with_nim_suffix = format!("{lib_name}.nim");

    let pkgs_path = installation.pkgs_path;
    let (lib_name_base, rest) = lib_name.split_once("/").unwrap_or((lib_name, ""));
    let dirs = read_dir(pkgs_path);
    let mut iter_result: Box<dyn Iterator<Item = PathBuf>> = Box::new(std::iter::empty());

    if let Ok(dirs) = dirs {
        // Start with user installed libraries
        if lib_name_base != "std" {
            for d in dirs {
                let Ok(d) = d else {
                    continue;
                };
                if d.path().starts_with(lib_name_base) {
                    let f = d.path().join(&lib_name_with_nim_suffix);
                    iter_result = Box::new(iter_result.chain(std::iter::once(f)));
                }
            }
        }
    }

    let lib_name_with_nim_suffix = if lib_name_base != "std" {
        lib_name_with_nim_suffix
    } else {
        format!("{rest}.nim") // strip std/ for standard libraries
    };

    // If not found, search for system libraries
    for sys_dir in installation.std_lib_paths {
        let f = sys_dir.join(&lib_name_with_nim_suffix);
        iter_result = Box::new(iter_result.chain(std::iter::once(f)));
    }
    iter_result
}

/// Given a nim library name, return the path to the corresponding library file.
pub fn get_lib_path(lib_name: &str) -> Option<PathBuf> {
    get_likely_lib_paths(lib_name).find(|p| p.exists())
}

pub fn get_system_lib_path() -> Option<PathBuf> {
    let installation = get_nim_installation_info();
    let sys_lib_path = installation.pkgs_path.join("system.nim");
    if sys_lib_path.exists() {
        Some(sys_lib_path)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_nim_installation_info() {
        let info = get_nim_installation_info();

        println!("{info:?}");
        assert!(!info.path.to_string_lossy().is_empty());
        assert!(!info.pkgs_path.to_string_lossy().is_empty());
        assert!(!info.version.is_empty());
        assert!(!info.std_lib_paths.is_empty());

        // eprintln!("Nim Info: {info:?}");
        // todo!();
    }

    #[test]
    fn test_get_lib_path() {
        let common_libs = ["strutils", "os", "math"];

        for lib_name in common_libs {
            let lib_path = get_lib_path(lib_name);
            if lib_path.is_none() {
                let searched_paths = get_likely_lib_paths(lib_name).collect::<Vec<_>>();
                eprintln!("Searched paths for {lib_name}: {searched_paths:?}");
            }
            assert!(
                lib_path.is_some(),
                "Expected to find library {lib_name} (it should be part of the standard Nim library), but got None.\n
                Information gathered about the Nim installation: {:?}",
                get_nim_installation_info()
            );
            let path = &lib_path.unwrap();
            assert!(
                path.ends_with(format!("{lib_name}.nim")),
                "Expected path to end with {lib_name}.nim, got: {path:?}"
            );
            assert!(
                path.exists(),
                "Expected the path to the library to exist, but it does not: {path:?}"
            );

            let as_string = path.to_string_lossy();
            assert!(
                as_string.contains("lib"),
                "Expected the path to contain 'lib', but it does not: {as_string}"
            );
        }

        // eprintln!("Library path: {path:?}");
    }

    #[test]
    fn test_get_lib_path_nonexistent() {
        let lib_path = get_lib_path("this_nim_lib_does_not_exist");
        assert!(
            lib_path.is_none(),
            "Expected no path for nonexistent library, got: {lib_path:?}"
        );
    }
}
