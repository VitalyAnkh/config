cd $HOME/projects/dev/cpp/taichi
#python3 setup.py clean
#python3 -m pip install --user -r requirements_dev.txt
export CLANG_PATH=$HOME/sdk/lib/taichi/clang+llvm-10.0.0-x86_64-linux-gnu-ubuntu-18.04/
export LLVM_PATH=$HOME/sdk/lib/taichi/taichi-llvm-10.0.0-linux
#. .venv/bin/activate
export LD=mold
export TAICHI_CMAKE_ARGS="-DCMAKE_CXX_COMPILER=${CLANG_PATH}/bin/clang++ -DCMAKE_EXPORT_COMPILE_COMMANDS=ON $TAICHI_CMAKE_ARGS"
export PATH="${LLVM_PATH}/bin:${CLANG_PATH}/bin:$PATH"
python setup.py develop --user
cp _skbuild/linux-x86_64-3.10/cmake-build/compile_commands.json .
