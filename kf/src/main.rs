mod lexer;
mod lang_def;

fn main() {
    let kulfon_lang = lang_def::Lang::new();
    lexer::tokenize(&kulfon_lang, "");
    println!("Hello, world!");
}
