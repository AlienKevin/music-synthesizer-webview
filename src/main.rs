extern crate web_view;

use web_view::*;

fn main() {
    web_view::builder()
        .title("Music synthesizer in webview")
        .content(Content::Html(include_str!("../dist/index.html")))
        .resizable(true)/*  */
        .debug(true)
        .user_data(())
        .invoke_handler(|_webview, _arg| Ok(()))
        .run()
        .unwrap();
}