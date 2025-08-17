use log::debug;
use serde_json::Value;
use tower_lsp::{
    LanguageServer, LspService, Server, jsonrpc,
    lsp_types::{
        DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
        DidChangeWorkspaceFoldersParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
        ExecuteCommandParams, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams,
        InitializeResult, InitializedParams, InlayHint, Location, OneOf, ReferenceParams,
        SaveOptions, SemanticTokensParams, SemanticTokensRangeParams, SemanticTokensRangeResult,
        SemanticTokensResult, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
        TextDocumentSyncOptions, TextDocumentSyncSaveOptions,
    },
};

pub mod nimy;

#[derive(Debug)]
struct Backend;

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult, jsonrpc::Error> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
            server_info: None,
            offset_encoding: None,
        })
    }
    async fn shutdown(&self) -> Result<(), jsonrpc::Error> {
        Ok(())
    }
    async fn initialized(&self, _: InitializedParams) {
        debug!("LSP Server Initialized");
    }
    async fn did_open(&self, _params: DidOpenTextDocumentParams) {}
    async fn did_change(&self, _params: DidChangeTextDocumentParams) {}
    async fn did_save(&self, _params: DidSaveTextDocumentParams) {}
    async fn goto_definition(
        &self,
        _params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>, jsonrpc::Error> {
        Ok(None)
    }
    async fn references(
        &self,
        _params: ReferenceParams,
    ) -> Result<Option<Vec<Location>>, jsonrpc::Error> {
        Ok(None)
    }
    async fn semantic_tokens_full(
        &self,
        _params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>, jsonrpc::Error> {
        Ok(None)
    }
    async fn semantic_tokens_range(
        &self,
        _params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>, jsonrpc::Error> {
        Ok(None)
    }
    async fn inlay_hint(
        &self,
        _params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>, jsonrpc::Error> {
        Ok(None)
    }
    async fn completion(
        &self,
        _params: tower_lsp::lsp_types::CompletionParams,
    ) -> Result<Option<tower_lsp::lsp_types::CompletionResponse>, jsonrpc::Error> {
        Ok(None)
    }
    async fn rename(
        &self,
        _params: tower_lsp::lsp_types::RenameParams,
    ) -> Result<Option<tower_lsp::lsp_types::WorkspaceEdit>, jsonrpc::Error> {
        Ok(None)
    }
    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        debug!("configuration changed!");
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        debug!("workspace folders changed!");
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        debug!("watched files have changed!");
    }

    async fn execute_command(
        &self,
        _: ExecuteCommandParams,
    ) -> Result<Option<Value>, jsonrpc::Error> {
        debug!("command executed!");
        Ok(None)
    }
}
#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|_client| Backend {}).finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
