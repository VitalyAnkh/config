bt := '0'

export RUST_BACKTRACE := bt

log := "warn"

set shell := ["fish", "-c"]

export JUST_LOG := log

all: llvm mold taichi ghc blender godot rust bevy perfbook chisel-book rocm ra wgpu wasmtime wlroots mutter riscv-gnu riscv-isa-sim emacs agda agda-stdlib eoc linux algoxy-book org

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

config_lean:
  #!/usr/bin/env bash
  #git clone https://github.com/leanprover/lean4 --recurse-submodules
  git config submodule.recurse true
  cd ~/projects/dev/lean/lean4
  mkdir -p build/release
  cd build/release
  LD=mold cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ../..

config_torch_mlir:
  #!/usr/bin/env bash
  cd ~/projects/dev/cpp/torch-mlir
  # -DPython3_FIND_VIRTUALENV=ONLY \
  # git submodule update --init
  #git submodule update --recursive
  #   -DMLIR_DIR="$LLVM_INSTALL_DIR/lib/cmake/mlir/" \
  # -DLLVM_DIR="$LLVM_INSTALL_DIR/lib/cmake/llvm/" \
  mkdir -p build
  git pull
  export LLVM_INSTALL_DIR=/home/vitalyr/projects/dev/cpp/llvm-vr/build
  CC=clang CXX=clang cmake -G Ninja -Bbuild \
  -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_EXTERNAL_PROJECTS="torch-mlir;torch-mlir-dialects" \
  -DLLVM_EXTERNAL_TORCH_MLIR_SOURCE_DIR="$PWD" \
  -DLLVM_EXTERNAL_TORCH_MLIR_DIALECTS_SOURCE_DIR="$PWD"/externals/llvm-external-projects/torch-mlir-dialects \
  -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
  -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_C_COMPILER_LAUNCHER=ccache -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
  -DCMAKE_EXE_LINKER_FLAGS_INIT="-fuse-ld=mold" -DCMAKE_MODULE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
  -DCMAKE_SHARED_LINKER_FLAGS_INIT="-fuse-ld=mold" \
  -DLIBTORCH_CACHE=ON \
  -DLLVM_TARGETS_TO_BUILD=host $HOME/projects/dev/cpp/llvm-vr/llvm
  cd build
  time ninja

lean:
  #!/usr/bin/env bash
  #git clone https://github.com/leanprover/lean4 --recurse-submodules
  cd ~/projects/dev/lean/lean4/build/release
  bear -- make -j20
  cp compile_commands.json ../../

config_llvm:
  #!/usr/bin/env bash
  echo "==== pull llvm-project ===="
  cd ~/projects/dev/cpp/llvm-project/
  # trash-put build
  mkdir -p build
  cd build
  cmake -G "Ninja" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DLLVM_USE_LINKER=mold \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
    -DLLVM_TARGETS_TO_BUILD="host;NVPTX" \
    -DLLVM_ENABLE_PROJECTS="clang;flang;llvm;mlir;clang-tools-extra;openmp" \
    -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
    -DLLVM_LIT_ARGS=-v \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DCMAKE_CXX_STANDARD=17 \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt;libc;libcxx;libcxxabi;libunwind" ../llvm

build_local_llvm:
  #!/usr/bin/env bash
  echo "==== build local llvm ===="
  cd ~/projects/dev/cpp/llvm-vr/
  git pull
  trash-put build
  mkdir -p build
  cd build
  CC=clang CXX=clang++ cmake -G "Ninja" ./ \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_USE_LINKER=mold \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
    -DLLVM_ENABLE_PROJECTS="mlir" \
    -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
    -DLLVM_LIT_ARGS=-v \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DCMAKE_CXX_STANDARD=17 ../llvm
  time ninja
  # trash-put $HOME/sdk/lib/llvm
  # cmake -DCMAKE_INSTALL_PREFIX=$HOME/sdk/lib/llvm -P cmake_install.cmake
  echo "==== build local llvm done ===="

triton:
  #!/usr/bin/env bash
  cd $HOME/projects/dev/cpp/triton
  mkdir -p build
  cd build
  cmake ../ -G Ninja \
  -DTRITON_BUILD_PYTHON_MODULE=ON \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DMLIR_DIR=$HOME/projects/dev/cpp/llvm-vr/build/lib/cmake/mlir \
  -DLLVM_DIR=$HOME/projects/dev/cpp/llvm-vr/build/lib/cmake/llvm \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DCMAKE_EXE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
  -DCMAKE_MODULE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
  -DCMAKE_SHARED_LINKER_FLAGS_INIT="-fuse-ld=mold" \

  #cmake --build .
  # test
  # pip install -e '.tests'
  # pytest -vs test/unit

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
  cd $HOME/projects/dev/cpp/circt/llvm/build
  # build llvm
  #ninja
  #ninja check-mlir
  cd $HOME/projects/dev/cpp/circt/build
  time ninja

config_circt:
  #!/usr/bin/env bash
  mkdir -p $HOME/projects/dev/cpp/circt/llvm/build
  cd $HOME/projects/dev/cpp/circt/
  git pull
  cd $HOME/projects/dev/cpp/circt/llvm/build
  # config llvm
  cmake -G Ninja ../llvm \
  -DLLVM_ENABLE_PROJECTS="mlir" \
  -DLLVM_TARGETS_TO_BUILD="host" \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
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
  scons platform=linuxbsd -j 12 target=editor compiledb=true linker=mold debug_symbols=yes
  #builtin_embree=no builtin_enet=no builtin_freetype=no builtin_graphite=no builtin_harfbuzz=no builtin_libogg=no builtin_libpng=no builtin_libtheora=no builtin_libvorbis=no builtin_libwebp=no builtin_mbedtls=no builtin_miniupnpc=no builtin_pcre2=no builtin_zlib=no builtin_zstd=no
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
  proxychains -q svn checkout https://svn.blender.org/svnroot/bf-blender/trunk/lib/linux_x86_64_glibc_228
  cd ~/projects/dev/c/blender-git/blender
  proxychains -q make update
  LD=mold make debug ninja
  #developer ccache
  cp ../build_linux_debug/compile_commands.json ./
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

algoxy-book:
  #!/usr/bin/env bash
  echo "==== pull algoxy-book ===="
  cd ~/projects/dev/tex/AlgoXY
  git pull
  git clean -fdx
  make
  cp algoxy-en.pdf ~/nutstore_files/Books/计算机科学/算法/
  cp algoxy-zh-cn.pdf ~/nutstore_files/Books/计算机科学/算法/
  echo "==== pull algoxy-book done ===="
