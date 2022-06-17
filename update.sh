echo "begin to pull codes and build books"

echo "==== pull llvm-project ===="
cd ~/projects/dev/cpp/llvm-project
git pull
cd build
CC=clang CXX=clang++ cmake -G "Ninja" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON\
  -DCMAKE_BUILD_TYPE=Debug -DLLVM_USE_LINKER=mold -DLLVM_TARGETS_TO_BUILD="X86"\
  -DLLVM_ENABLE_PROJECTS="clang;llvm;mlir;clang-tools-extra"\
  -DLLVM_OPTIMIZED_TABLEGEN=ON ../llvm
echo "==== pull llvm-project done ===="

echo "==== pull taichi ===="
cd ~/projects/dev/cpp/taichi
export TAICHI_CMAKE_ARGS="-DCMAKE_CXX_COMPILER=clang++ $TAICHI_CMAKE_ARGS"
export DEBUG=1
# python3 setup.py clean
#python3 -m pip install --user -r requirements_dev.txt
git pull --recurse-submodules
python3 setup.py develop --user
echo "==== pull llvm-project done ===="

# echo "==== pull Unreal Engine ===="
# cd ~/projects/dev/cpp/UnrealEngine
# git pull
# cd build
# CC=clang CXX=clang++ cmake -G "Ninja" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON\
#   -DCMAKE_BUILD_TYPE=Debug -DLLVM_USE_LINKER=mold -DLLVM_TARGETS_TO_BUILD="X86"\
#   -DLLVM_ENABLE_PROJECTS="clang;llvm;mlir;clang-tools-extra"\
#   -DLLVM_OPTIMIZED_TABLEGEN=ON ../llvm
# echo "==== pull Unreal Engine done ===="

echo "==== pull Pilot ===="
cd ~/projects/dev/cpp/Pilot
git pull
./build_linux.sh debug
echo "==== pull Pilot done ===="

echo "==== pull godot ===="
cd ~/projects/dev/cpp/godot
git pull
echo "==== pull godot done ===="

echo "==== pull v8 riscv-collab/RV32G ===="
cd ~/projects/dev/cpp/v8/
gclient sync
cd v8
git pull
ninja -C out/riscv32.debug -t compdb cxx cc > compile_commands.json
echo "==== pull v8 done ===="


echo "==== pull wgpu ===="
cd ~/projects/dev/rust-projects/wgpu
git pull
echo "==== pull wgpu done ===="

echo "==== pull mesa ===="
cd ~/projects/dev/c/mesa
git pull
cd build
meson --reconfigure ..
echo "==== pull mesa done ===="


echo "==== pull iced ===="
cd ~/projects/dev/rust-projects/iced
git pull
echo "==== pull iced done ===="

echo "==== pull wlroots ===="
cd ~/projects/dev/c/wlroots
git pull
meson build --reconfigure
echo "==== pull wlroots done ===="

echo "==== pull mutter ===="
cd ~/projects/dev/c/mutter
git pull
meson build --reconfigure
echo "==== pull wlroots done ===="

echo "==== pull mutter ===="
cd ~/projects/dev/c/riscv-isa-sim
git pull
mkdir build
cd build
../configure --prefix=$RISCV
bear -- make -j12
echo "==== pull wlroots done ===="

echo "==== pull blender ===="
cd ~/projects/dev/c/blender-git/blender
git pull
meson build --reconfigure
echo "==== pull blender done ===="


echo "==== pull redox ===="
cd ~/projects/dev/rust-projects/redox
make clean
make pull
make all
echo "==== pull redox done ===="

echo "==== pull rust ===="
cd ~/projects/dev/rust-projects/rust
git pull
echo "==== pull rust done ===="

echo "==== pull deno ===="
cd ~/projects/dev/rust-projects/deno
git pull
echo "==== pull deno done ===="

echo "==== pull agda ===="
cd ~/sdk/lib/agda-stdlib
git pull
echo "==== pull agda done ===="

echo "=== pull emacs ==="
cd ~/projects/dev/emacs
git pull
./autogen.sh
CFLAGS="-ggdb3 -O0" CXXFLAGS="-ggdb3 -O0" LDFLAGS="-ggdb3" ./configure --with-modules --with-json
bear -- make -j12
echo "=== pull emacs done ==="

echo "=== pull linux ==="
cd ~/projects/dev/linux
git pull
make defconfig
bear -- make -j12
echo "=== pull linux done ==="

echo "==== pull iced ===="
cd ~/projects/dev/rust-projects/iced
git pull
echo "==== pull iced done ===="

echo "==== pull rust-analyzer ===="
cd ~/projects/dev/rust-projects/rust-analyzer
git pull
echo "==== pull rust-analyzer done ====" 

echo "==== pull bevy ===="
cd ~/projects/dev/bevy
git pull
echo "==== pull bevy done ===="

echo "==== pull perfbook ===="
cd ~/projects/dev/book/perfbook
git pull
make 1c
cp perfbook-1c.pdf ~/nutstore_files/Books/计算机科学/计算机底层/
echo "==== pull perfbook done ===="

echo "==== pull Essentials-of-Compilation ===="
cd ~/projects/dev/book/Essentials-of-Compilation
git pull
make all
echo "==== pull Essentials-of-Compilation done ===="