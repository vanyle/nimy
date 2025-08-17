/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { type ExtensionContext } from "vscode";

import {
  type Executable,
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export async function activate(context: ExtensionContext) {
  // const outputChannel = window.createOutputChannel("Nimy Language Server");
  const command = process.env.SERVER_PATH || "nimy";

  const run: Executable = {
    command,
    transport: 0,
    options: {
      shell: true,
      env: {
        ...process.env,
        RUST_LOG: "debug",
      },
    },
  };

  const serverOptions: ServerOptions = {
    run: run,
    debug: run,
  };

  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "nim" }],
    // Notify the server about file changes to '.clientrc files contained in the workspace
    //synchronize: {
    //  fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    //},
  };

  client = new LanguageClient(
    "nimy-language-server",
    "Nimy Language Server",
    serverOptions,
    clientOptions,
    true
  );
  client.info(`Starting client with command: ${command}`);
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
