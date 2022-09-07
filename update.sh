echo "begin to pull codes and build books"

echo "==== pull llvm-project ===="
cd ~/projects/dev/cpp/llvm-project
git pull
cd build
CC=clang CXX=clang++ cmake -G "Ninja" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=Debug -DLLVM_USE_LINKER=mold -DLLVM_TARGETS_TO_BUILD="X86" \
  -DLLVM_ENABLE_PROJECTS="clang;llvm;mlir;clang-tools-extra;libc;libcxx;libcxxabi;libunwind" \
  -DLLVM_OPTIMIZED_TABLEGEN=ON ../llvm
echo "==== pull llvm-project done ===="

echo "==== pull ROCm-Device-Libs ===="
cd ~/projects/dev/cpp/ROCm-Device-Libs
git pull
cd build
export LLVM_BUILD=$HOME/projects/dev/cpp/llvm-project/build
cmake -G "Ninja" -DCMAKE_PREFIX_PATH=$LLVM_BUILD -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DLLVM_USE_LINKER=mold ..
#ninja all
echo "==== pull ROcm-Device-Libs done ===="

echo "==== pull taichi ===="
cd $HOME/projects/dev/cpp/taichi
git pull --recurse-submodules
echo "==== pull taichi done ===="

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
scons platform=linuxbsd -j 12 target=release_debug compiledb=true
echo "==== pull godot done ===="

echo "==== pull v8 riscv-collab/RV32G ===="
cd ~/projects/dev/cpp/v8/
gclient sync
cd v8
git pull
ninja -C out/riscv32.debug -t compdb cxx cc >compile_commands.json
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
cd ~/projects/dev/c/blender-git/lib
svn checkout https://svn.blender.org/svnroot/bf-blender/trunk/lib/linux_centos7_x86_64
cd ~/projects/dev/c/blender-git/blender
make update
make debug developer ccache ninja
cp ../build_linux_debug/compile_commands.json .
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

echo "==== pull wasmtime ===="
cd ~/projects/dev/rust-projects/wasmtime
git pull --recurse-submodules
echo "==== pull wasmtime done ===="

echo "==== pull riscv-gnu-toolchain ===="
cd ~/projects/dev/c/riscv-gnu-toolchain
git pull --recurse-submodules
./configure --prefix=/opt/riscv
make linux -j12
echo "==== pull riscv-gnu-toolchain done ===="

echo "==== pull deno ===="
cd ~/projects/dev/rust-projects/deno
git pull
echo "==== pull deno done ===="

echo "==== pull agda-stdlib ===="
cd ~/sdk/lib/agda-stdlib
git pull
echo "==== pull agda done ===="

echo "==== pull agda ===="
cd ~/projects/dev/haskell/agda
git pull --recurse-submodules
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
chmod 0600 /home/vitalyr/nutstore_files/Books/计算机科学/编译原理/essentials-of-compilation.pdf
cp book.pdf ~/nutstore_files/Books/计算机科学/编译原理/essentials-of-compilation.pdf
echo "==== pull Essentials-of-Compilation done ===="

echo "==== pull chisel-book ===="
cd ~/projects/dev/scala-projects/chisel-book
git pull
make
cp chisel-book.pdf ~/nutstore_files/Books/计算机科学/计算机体系结构/
echo "==== pull chisel-book done ===="
