#[macro_use] extern crate quote;
extern crate proc_macro2;
use std::env;
use std::fs::{File, self};
use std::io::Write;
use std::path::{Path, PathBuf};

fn create_parser_test(fn_name: proc_macro2::Ident, in_path: &str, check_path: &str) -> proc_macro2::TokenStream {
    quote! {
        #[test]
        fn #fn_name() -> Result<(), failure::Error> {
            let in_path = std::path::Path::new(#in_path);
            let check_path = std::path::Path::new(#check_path);
            let data = std::fs::read_to_string(&in_path)?;
            let output = yaplwrs::parse(&data).expect(&format!(
                "failed to parse `{}`",
                &in_path.to_string_lossy()
            ));

            match read_to_string(&check_path) {
                Ok(out_compare) => assert_eq!(
                    output.to_string(),
                    out_compare,
                    "incorrect output for `{}`",
                     #in_path
                ),
                Err(_) => write!(std::fs::File::create(check_path).unwrap(), "{}", output)?,
            };

            Ok(())
        }
    }
}

fn create_parser_tests(test_dir: &Path) -> proc_macro2::TokenStream {
    let input_dir = test_dir.join("pass");
    let mut tokens = proc_macro2::TokenStream::new();
    for file in fs::read_dir(input_dir).unwrap() {
        let file = file.unwrap();
        let in_path = file.path();
        let check_path = test_dir.join("pass-output").join(file.file_name());
        let fn_name = &in_path.file_stem().unwrap();
        let fn_name = &*fn_name.to_string_lossy();
        let fn_name = fn_name.replace("-", "_");
        tokens.extend(create_parser_test(format_ident!("{}", fn_name), &in_path.to_string_lossy(), &check_path.to_string_lossy()));
    }

    tokens
}

fn generate_tests() {
    let out_dir = env::var("OUT_DIR").unwrap();

    let parser_tests = Path::new(&out_dir).join("tests/parser");

    fs::create_dir_all(&parser_tests).unwrap();

    let dest_path = &parser_tests.join("pass.rs");
    let mut f = File::create(&dest_path).unwrap();

    let parser_tests_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR")
        .expect("current dir is nonexistant?"))
        .join("tests/parser");

    // todo: make this "crate-root" or whatever
    let tests = create_parser_tests(&parser_tests_dir);

    write!(f, "{}", tests).unwrap();
}

fn main() {
    generate_tests()
}