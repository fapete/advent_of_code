/*
 * Common Lib to extend with functions/structs I expect to be reusing on other days.
 */
pub mod aoclib {
    use std::fs::File;
    use std::io::prelude::*;

    pub fn read_file(fname: &str) -> std::io::Result<String> {
        let mut file = File::open(fname)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        return Ok(content);
    }
}
