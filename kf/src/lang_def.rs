const KULFON_KEYWORDS: [&str; 1] = ["fn"];
const KULFON_RES_KEYWORDS: [&str; 0] = [];
const KULFON_SPEC_SYMBOLS: [&str; 2] = ["+", "++"];

#[derive(Clone)]
pub struct Range {
    pub start: String,
    pub end: String,
    pub exceptions: Vec<String>,
}

#[derive(Clone)]
pub enum RangeBased {
    LineComment(Range),
    DocComment(Range),
    Comment(Range),
    String(Range),
    RawString(Range),
}

impl RangeBased {
    pub fn get_range(&self) -> &Range {
        match self {
            RangeBased::LineComment(r) => r,
            RangeBased::DocComment(r) => r,
            RangeBased::Comment(r) => r,
            RangeBased::String(r) => r,
            RangeBased::RawString(r) => r,
        }
    }
}

pub struct Lang {
    pub keywords: Vec<String>,
    pub reserved_keywords: Vec<String>,
    pub special_sym: Vec<String>,
    pub range_based: Vec<RangeBased>,
}

impl Lang {
    pub fn new() -> Lang {
        let mut sym = KULFON_SPEC_SYMBOLS
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>();
        // sort by length starting from the longer symobol
        // that order is needed by lexer
        sym.sort_by(|a, b| b.len().cmp(&a.len()));

        Lang {
            keywords: KULFON_KEYWORDS.iter().map(|s| s.to_string()).collect(),
            reserved_keywords: KULFON_RES_KEYWORDS.iter().map(|s| s.to_string()).collect(),
            special_sym: sym,
            range_based: vec![
                RangeBased::LineComment(Range {
                    start: "//".into(),
                    end: "\n".into(),
                    exceptions: Vec::new(),
                }),
                RangeBased::Comment(Range {
                    start: "/*".into(),
                    end: "*/".into(),
                    exceptions: Vec::new(),
                }),
                RangeBased::String(Range {
                    start: "\"".into(),
                    end: "\"".into(),
                    exceptions: vec!["\\\"".into()],
                }),
                RangeBased::RawString(Range {
                    start: "r#\"".into(),
                    end: "\"#".into(),
                    exceptions: Vec::new(),
                }),
            ],
        }
    }
}

#[derive(Copy, Clone)]
pub struct TextPoint {
    pub line: usize,
    pub col: usize,
}

pub struct ParsingError {
    pub msg: String,
    pub details: String,
    pub at: TextPoint,
}
