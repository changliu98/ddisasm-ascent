use ddisasm_ascent::ddisasm::ast::{read_cfg, DatalogCFG};




fn main() {
    let mut db = DatalogCFG::default();
    
    read_cfg(&mut db, "sample");
    db.run();


}
