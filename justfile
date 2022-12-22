bt := '0'

export RUST_BACKTRACE := bt

log := "warn"

set shell := ["fish", "-c"]

export JUST_LOG := log

all: llvm mold taichi ghc blender godot rust bevy perfbook chisel-book rocm ra wgpu wasmtime wlroots mutter riscv-gnu riscv-isa-sim emacs agda agda-stdlib eoc linux

llvm:
  #!/usr/bin/env bash
  echo "==== pull llvm-project ===="
  cd ~/projects/dev/cpp/llvm-project
  git pull
  cd ~/projects/dev/emacs-projects/llvm-tools
  cp ~/projects/dev/cpp/llvm-project/llvm/utils/emacs/*.el ./
  git add -A
  git commit -m "up"
  git push
  echo "==== pull llvm-project done ===="

config_llvm:
  #!/usr/bin/env bash
  echo "==== pull llvm-project ===="
  cd ~/projects/dev/cpp/llvm-project/
  trash-put build
  mkdir -p build
  cd build
  CC=clang CXX=clang++ cmake -G "Ninja" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DLLVM_USE_LINKER=mold \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
    -DLLVM_TARGETS_TO_BUILD="host;NVPTX" \
    -DLLVM_ENABLE_PROJECTS="clang;flang;llvm;mlir;clang-tools-extra;openmp" \
    -DLLVM_LIT_ARGS=-v \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DCMAKE_CXX_STANDARD=17 \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt;libc;libcxx;libcxxabi;libunwind" ../llvm


trash_emacs_cache:
  #!/usr/bin/env bash
  trash-put $HOME/.config/.emacs.d/.local/straight/build-*
  trash-put $HOME/.config/.emacs.d/.local/autoloads*
  trash-put $HOME/.config/.emacs.d/eln-cache
  trash-put $HOME/.config/.emacs.d/.local/cache/eln
  trash-put $HOME/.config/.emacs.d/.local/etc/@

build_local_emacs:
  #!/usr/bin/env bash
  cd $HOME/projects/aur/emacs-pgtk-git/src/emacs-git
  make bootstrap-clean
  make clean
  cd $HOME/projects/aur/emacs-pgtk-git/
  trash-put ./src/emacs-git/lisp/*elc
  trash-put ./src/emacs-git/lisp/progmodes/*elc
  trash-put ./src/emacs-git/lisp/emacs-lisp/*elc
  mksrcinfo
  #makepkg -si
  #proxychains -q $HOME/.config/.emacs.d/bin/doom upgrade --force

test_local_emacs: trash_emacs_cache build_local_emacs

clean_rust:
  #!/usr/bin/env bash
  trash-put $HOME/sdk/build/cache/*

org:
  #!/usr/bin/env bash
  cd $HOME/org
  git add -A
  git commit -m "up"
  git push

build_taichi:
  #!/usr/bin/env bash
  cd $HOME/projects/dev/cpp/taichi
  #python3 setup.py clean
  #python3 -m pip install --user -r requirements_dev.txt
  export CLANG_PATH=$HOME/sdk/lib/taichi/clang+llvm-10.0.0-x86_64-linux-gnu-ubuntu-18.04/
  export LLVM_PATH=$HOME/sdk/lib/taichi/taichi-llvm-10.0.0-linux
  #. .venv/bin/activate
  export LD=mold
  export TAICHI_CMAKE_ARGS="-DCMAKE_CXX_COMPILER=${CLANG_PATH}/bin/clang++   -DCMAKE_EXPORT_COMPILE_COMMANDS=ON $TAICHI_CMAKE_ARGS"
  export PATH="${LLVM_PATH}/bin:${CLANG_PATH}/bin:$PATH"
  python setup.py develop --user
  cp _skbuild/linux-x86_64-3.10/cmake-build/compile_commands.json .

mold:
  #!/usr/bin/env bash
  cd $HOME/projects/dev/cpp/mold
  git pull

config_mold:
  #!/usr/bin/env bash
  cd $HOME/projects/dev/cpp/mold
  mkdir build
  cd build
  CXXFLAGS="-fuse-ld=mold" CC=clang CXX=clang++ cmake -G "Ninja" \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo ../

build_circt:
  #!/usr/bin/env bash
  mkdir -p $HOME/projects/dev/cpp/circt/llvm/build
  cd $HOME/projects/dev/cpp/circt/llvm/build
  cmake -G Ninja ../llvm \
  -DLLVM_ENABLE_PROJECTS="mlir" \
  -DLLVM_TARGETS_TO_BUILD="host" \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
  ninja
  ninja check-mlir
  # build circt
  mkdir -p $HOME/projects/dev/cpp/circt/build
  cd $HOME/projects/dev/cpp/circt/build
  cmake -G Ninja .. \
  -DMLIR_DIR=../llvm/build/lib/cmake/mlir \
  -DLLVM_DIR=../llvm/build/lib/cmake/llvm \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
  ninja
  ninja check-circt

rocm:
  #!/usr/bin/env bash
  echo "==== pull ROCm-Device-Libs ===="
  cd ~/projects/dev/cpp/ROCm-Device-Libs
  git pull
  cd build
  export LLVM_BUILD=$HOME/projects/dev/cpp/llvm-project/build
  cmake -G "Ninja" -DCMAKE_PREFIX_PATH=$LLVM_BUILD -DCMAKE_EXPORT_COMPILE_COMMANDS=ON   -DLLVM_USE_LINKER=mold ..
  #ninja all
  echo "==== pull ROcm-Device-Libs done ===="

taichi:
  #!/usr/bin/env bash
  echo "==== pull taichi ===="
  cd $HOME/projects/dev/cpp/taichi
  git pull --recurse-submodules
  echo "==== pull taichi done ===="

ghc:
  #!/usr/bin/env bash
  echo "==== pull ghc ===="
  cd $HOME/projects/dev/haskell/ghc
  git pull --recurse-submodules
  ./boot
  ./configure
  bear -- make -j20
  make install -j20
  echo "==== pull ghc done ===="

# echo "==== pull Unreal Engine ===="
# cd ~/projects/dev/cpp/UnrealEngine
# git pull
# cd build
# CC=clang CXX=clang++ cmake -G "Ninja" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON\
#   -DCMAKE_BUILD_TYPE=Release -DLLVM_USE_LINKER=mold -DLLVM_TARGETS_TO_BUILD="X86"\
#   -DLLVM_ENABLE_PROJECTS="clang;flang;llvm;mlir;clang-tools-extra;libcxx;libcxxabi"\
#   -DLLVM_OPTIMIZED_TABLEGEN=ON ../llvm
# echo "==== pull Unreal Engine done ===="

pilot:
  #!/usr/bin/env bash
  echo "==== pull Pilot ===="
  cd ~/projects/dev/cpp/Pilot
  git pull
  ./build_linux.sh debug
  echo "==== pull Pilot done ===="

godot:
  #!/usr/bin/env bash
  echo "==== pull godot ===="
  cd ~/projects/dev/cpp/godot
  git pull
  scons platform=linuxbsd -j 12 target=release_debug compiledb=true
  echo "==== pull godot done ===="

v8:
  #!/usr/bin/env bash
  echo "==== pull v8 riscv-collab/RV32G ===="
  cd ~/projects/dev/cpp/v8/
  gclient sync
  cd v8
  git pull
  ninja -C out/riscv32.debug -t compdb cxx cc >compile_commands.json
  echo "==== pull v8 done ===="

wgpu:
  #!/usr/bin/env bash
  echo "==== pull wgpu ===="
  cd ~/projects/dev/rust-projects/wgpu
  git pull
  echo "==== pull wgpu done ===="

mesa:
  #!/usr/bin/env bash
  echo "==== pull mesa ===="
  cd ~/projects/dev/c/mesa
  git pull
  cd build
  meson --reconfigure .. -Dgallium-rusticl=true -Dopencl-spirv=true -Dshader-cache=true -Dllvm=true
  echo "==== pull mesa done ===="

iced:
  #!/usr/bin/env bash
  echo "==== pull iced ===="
  cd ~/projects/dev/rust-projects/iced
  git pull
  echo "==== pull iced done ===="

wlroots:
  #!/usr/bin/env bash
  echo "==== pull wlroots ===="
  cd ~/projects/dev/c/wlroots
  git pull
  meson build --reconfigure
  echo "==== pull wlroots done ===="

mutter:
  #!/usr/bin/env bash
  echo "==== pull mutter ===="
  cd ~/projects/dev/c/mutter
  git pull
  meson build --reconfigure
  echo "==== pull mutter done ===="

riscv-isa-sim:
  #!/usr/bin/env bash
  echo "==== pull riscv-isa-sim ===="
  cd ~/projects/dev/c/riscv-isa-sim
  git pull
  mkdir build
  cd build
  ../configure --prefix=$RISCV
  bear -- make -j12
  echo "==== pull riscv-isa-sim done ===="

blender:
  #!/usr/bin/env bash
  echo "==== pull blender ===="
  cd ~/projects/dev/c/blender-git/lib
  svn checkout https://svn.blender.org/svnroot/bf-blender/trunk/lib/linux_centos7_x86_64
  cd ~/projects/dev/c/blender-git/blender
  make update
  make debug developer ccache ninja
  cp ../build_linux_debug/compile_commands.json .
  echo "==== pull blender done ===="

redox:
  #!/usr/bin/env bash
  echo "==== pull redox ===="
  cd ~/projects/dev/rust-projects/redox
  make clean
  make pull
  make all
  echo "==== pull redox done ===="

rust:
  #!/usr/bin/env bash
  echo "==== pull rust ===="
  cd ~/projects/dev/rust-projects/rust
  git pull
  echo "==== pull rust done ===="

wasmtime:
  #!/usr/bin/env bash
  echo "==== pull wasmtime ===="
  cd ~/projects/dev/rust-projects/wasmtime
  git pull --recurse-submodules
  echo "==== pull wasmtime done ===="

riscv-gnu:
  #!/usr/bin/env bash
  echo "==== pull riscv-gnu-toolchain ===="
  cd ~/projects/dev/c/riscv-gnu-toolchain
  git pull --recurse-submodules
  ./configure --prefix=/opt/riscv
  make linux -j12
  echo "==== pull riscv-gnu-toolchain done ===="

deno:
  #!/usr/bin/env bash
  echo "==== pull deno ===="
  cd ~/projects/dev/rust-projects/deno
  git pull
  echo "==== pull deno done ===="

agda-stdlib:
  #!/usr/bin/env bash
  echo "==== pull agda-stdlib ===="
  cd ~/sdk/lib/agda-stdlib
  git pull
  echo "==== pull agda done ===="

agda:
  #!/usr/bin/env bash
  echo "==== pull agda ===="
  cd ~/projects/dev/haskell/agda
  git pull --recurse-submodules
  echo "==== pull agda done ===="

emacs:
  #!/usr/bin/env bash
  echo "=== pull emacs ==="
  cd ~/projects/dev/emacs
  mkdir -p build
  git pull
  ./autogen.sh
  CFLAGS="-ggdb3 -O0" CXXFLAGS="-ggdb3 -O0" LDFLAGS="-ggdb3" ./configure --with-modules \
  --with-json \
  --with-pgtk \
  --with-xwidgets
  bear -- make DESTDIR=./build -j20
  echo "=== pull emacs done ==="

linux:
  #!/usr/bin/env bash
  echo "=== pull linux ==="
  cd ~/projects/dev/linux
  git pull
  make defconfig
  bear -- make -j12
  echo "=== pull linux done ==="

ra:
  #!/usr/bin/env bash
  echo "==== pull rust-analyzer ===="
  cd ~/projects/dev/rust-projects/rust-analyzer
  git pull
  echo "==== pull rust-analyzer done ===="

bevy:
  #!/usr/bin/env bash
  echo "==== pull bevy ===="
  cd ~/projects/dev/bevy
  git pull
  echo "==== pull bevy done ===="

perfbook:
  #!/usr/bin/env bash
  echo "==== pull perfbook ===="
  cd ~/projects/dev/book/perfbook
  git pull
  make 1c
  cp perfbook-1c.pdf ~/nutstore_files/Books/计算机科学/计算机底层/
  echo "==== pull perfbook done ===="

eoc:
  #!/usr/bin/env bash
  echo "==== pull Essentials-of-Compilation ===="
  cd ~/projects/dev/book/Essentials-of-Compilation
  git pull
  make all
  chmod 0600 /home/vitalyr/nutstore_files/Books/计算机科学/编译原理/essentials-of-compilation.pdf
  cp book.pdf ~/nutstore_files/Books/计算机科学/编译原理/essentials-of-compilation.pdf
  echo "==== pull Essentials-of-Compilation done ===="

chisel-book:
  #!/usr/bin/env bash
  echo "==== pull chisel-book ===="
  cd ~/projects/dev/scala-projects/chisel-book
  git pull
  make
  cp chisel-book.pdf ~/nutstore_files/Books/计算机科学/计算机体系结构/
  echo "==== pull chisel-book done ===="
