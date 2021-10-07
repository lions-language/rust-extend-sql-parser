#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

macro_rules! kw_def {
    ($ident:ident = $string_keyword:expr) => {
        pub const $ident: &'static str = $string_keyword;
    };
    ($ident:ident) => {
        kw_def!($ident = stringify!($ident));
    };
}

macro_rules! define_keywords {
    ($(
        $ident:ident $(= $string_keyword:expr)?
    ),*) => {
        pub enum Keyword {
            NoKeyword,
            $($ident),*
        }

        pub const ALL_KEYWORDS_INDEX: &[Keyword] = &[
            $(Keyword::$ident),*
        ];

        // define all keyword static-string
        $(kw_def!($ident $(= $string_keyword)?);)*

        pub const ALL_KEYWORDS: &[&'static str] = &[
            $($ident),*
        ];
    }
}

define_keywords!(
    ABORT
);

