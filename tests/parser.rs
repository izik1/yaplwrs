use std::fs::{read_dir, read_to_string, File};
use std::io::Write;
use yaplwrs::parse;

#[test]
fn test() -> Result<(), failure::Error> {
    let path = std::env::current_dir()
        .expect("current dir is nonexistant?")
        .join("tests/parser");

    let input_dir = path.join("pass");
    for file in read_dir(input_dir)? {
        let file = file?;

        let data = read_to_string(file.path())?;

        let output = parse(&data).expect(&format!(
            "failed to parse `{}`",
            file.path().to_string_lossy()
        ));
        let out_compare_path = path.join("pass-output").join(file.file_name());

        match read_to_string(out_compare_path.clone()) {
            Ok(out_compare) => assert_eq!(
                output.to_string(),
                out_compare,
                "incorrect output for `{}`",
                file.path().to_string_lossy()
            ),
            Err(_) => write!(File::create(out_compare_path).unwrap(), "{}", output)?,
        };
    }

    Ok(())
}
