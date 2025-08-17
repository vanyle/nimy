import { defineConfig, UserConfig } from "vite";

export default defineConfig({
  root: "client",
  esbuild: {
    sourcemap: true,
    target: "es2024",
    format: "cjs",
    sourcesContent: false,
    platform: "neutral",
  },
  build: {
    lib: {
      entry: "./src/extension.ts",
      formats: ["cjs"],
      fileName: (format, entry) => "extension.js",
    },
    target: "es2024",
    rollupOptions: {
      external: ["vscode"],
    },
    minify: false,
    emptyOutDir: true,
    sourcemap: true,
    outDir: "out",
  },
  plugins: [],
} satisfies UserConfig);
