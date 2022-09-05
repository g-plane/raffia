use raffia::{ast::Stylesheet, Parser};
use wasm_bindgen::prelude::*;

#[wasm_bindgen(js_name = parseStylesheet)]
pub fn parse_stylesheet(source: String, syntax: JsValue) -> Result<JsValue, JsValue> {
    let syntax = serde_wasm_bindgen::from_value(syntax)?;
    let mut parser = Parser::new(&source, syntax);
    match parser.parse::<Stylesheet>() {
        Ok(ast) => serde_wasm_bindgen::to_value(&ast).map_err(JsValue::from),
        Err(error) => Err(serde_wasm_bindgen::to_value(&(
            &error,
            error.kind.to_string(),
        ))?),
    }
}
