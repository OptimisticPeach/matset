rm -rf ./package/
mkdir ./package/

cargo build --release --target wasm32-unknown-unknown
cp ./target/wasm32-unknown-unknown/release/matset.wasm ./package/
 
cp ./typst.toml ./package/
cp ./typst_src/* ./package/
cp ./LICENSE ./package/
cp ./README.md ./package/
