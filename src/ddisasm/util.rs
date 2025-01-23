use std::borrow::Borrow;



pub fn read_csv<T>(path: &str) -> impl Iterator<Item = T>
where
   for<'de> T: serde::de::Deserialize<'de>,
{
    // read csv file replace all hex values with decimal

    println!("Reading CSV file: {}", path);
    // check if file exists
    let path_replace = if !std::path::Path::new(path).exists() {
        // replace the .csv with .facts
        path.replace(".csv", ".facts")
    } else {
        path.to_string()
    };
    let path = path_replace.as_str();
        
    csv::ReaderBuilder::new()
        .delimiter(b'\t')
        .has_headers(false)
        .double_quote(false)
        .quoting(false)
        .from_path(path)
        .unwrap()
        .into_deserialize()
        .map(|x| x.unwrap())
}

pub fn leak<T: Borrow<TB> + 'static, TB: ?Sized>(x: T) -> &'static TB {
    let leaked: &'static T = Box::leak(Box::new(x));
    leaked.borrow()
}

