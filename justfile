bt := '0'

export RUST_BACKTRACE := bt
export LLVM_INSTALL_DIR := "/home/vitalyr/sdk/lib/llvm"

log := "warn"

set shell := ["fish", "-c"]

export JUST_LOG := log

all: servo mpv llvm mold taichi ghc blender godot rust bevy perfbook chisel-book rocm ra wgpu wasmtime wlroots mutter riscv-gnu riscv-isa-sim emacs agda agda-stdlib eoc linux algoxy-book org verilator yosys egui

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
  git pull
  mkdir -p build
  cd build
  LD=mold cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ../
  cp compile_commands.events.json compile_commands.json

config_torch_mlir_with_external_project:
  #!/usr/bin/env bash
  echo "==== config torch-mlir ===="
  cd ~/projects/dev/cpp/torch-mlir
  # -DPython3_FIND_VIRTUALENV=ONLY \
  # git submodule update --init
  #-DLLVM_EXTERNAL_TORCH_MLIR_DIALECTS_SOURCE_DIR="$PWD"/externals/llvm-external-projects/torch-mlir-dialects \
    #-DLLVM_EXTERNAL_PROJECTS="torch-mlir;torch-mlir-dialects" \
    #-DLLVM_EXTERNAL_TORCH_MLIR_SOURCE_DIR="$PWD"\
  #git submodule update --recursive
  git pull
  mkdir -p build
  cmake -G Ninja -B build \
    -DCMAKE_BUILD_TYPE=Release \
    -DMLIR_DIR="$LLVM_INSTALL_DIR/lib/cmake/mlir/" \
    -DLLVM_DIR="$LLVM_INSTALL_DIR/lib/cmake/llvm/" \
    -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
    -DCMAKE_EXE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_MODULE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_SHARED_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DLIBTORCH_CACHE=ON \
    -DLLVM_TARGETS_TO_BUILD=host ./
  # cd build
  # time ninja
  echo "==== config torch-mlir done ===="

config_torch_mlir:
  #!/usr/bin/env bash
  echo "==== config torch-mlir ===="
  cd ~/projects/dev/cpp/torch-mlir
  pwd
  git submodule update --recursive
  git pull
  # Set the variant of libtorch to build link against. `shared`|`static` and optionally `cxxabi11`
  trash-put build
  mkdir -p build
  cmake -G Ninja -B build \
  -DCMAKE_BUILD_TYPE=Debug \
  -DPython3_FIND_VIRTUALENV=ONLY \
  -DLLVM_ENABLE_PROJECTS=mlir \
  -DLLVM_EXTERNAL_PROJECTS="torch-mlir;torch-mlir-dialects" \
  -DLLVM_EXTERNAL_TORCH_MLIR_SOURCE_DIR="$PWD" \
  -DLLVM_EXTERNAL_TORCH_MLIR_DIALECTS_SOURCE_DIR="$PWD"/externals/llvm-external-projects/torch-mlir-dialects \
  -DLIBTORCH_SRC_BUILD=ON \
  -DLIBTORCH_VARIANT=shared \
  -DLLVM_USE_LINKER=mold \
  -DCMAKE_EXE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
  -DCMAKE_MODULE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
  -DCMAKE_SHARED_LINKER_FLAGS_INIT="-fuse-ld=mold" \
  -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
  -DLLVM_TARGETS_TO_BUILD=host externals/llvm-project/llvm
  echo "==== config torch-mlir done ===="

lean:
  #!/usr/bin/env bash
  #git clone https://github.com/leanprover/lean4 --recurse-submodules
  cd ~/projects/dev/lean/lean4/build/
  bear -- make -j10

deploy_emacs:
  #!/usr/bin/env bash
  cd ~/
  zip -r emacs.d.zip .emacs.d
  mv ~/emacs.d.zip ~/nutstore_files/Work/emacs.d.zip


config_latest_llvm:
  #!/usr/bin/env bash
  echo "==== config llvm-project ===="
  cd $HOME/projects/dev/cpp/llvm-project/
  git pull
  trash-put build
    # -DCLANG_DEFAULT_CXX_STDLIB=libc++ \
    # -DCMAKE_CXX_COMPILER=clang++ \
    # -DCMAKE_C_COMPILER=clang \
  cmake -G Ninja -B build ./llvm \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_CXX_COMPILER=/usr/local/opt/llvm@18/bin/clang++ \
    -DCMAKE_C_COMPILER=/usr/local/opt/llvm@18/bin/clang \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local/opt/llvm@18 \
    -DMLIR_ENABLE_CUDA_RUNNER=ON \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
    -DLLVM_TARGETS_TO_BUILD="X86;NVPTX;RISCV;AMDGPU" \
    -DLLVM_ENABLE_PROJECTS="clang;flang;llvm;mlir;clang-tools-extra;lldb" \
    -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
    -DLLVM_LIT_ARGS=-v \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_INSTALL_UTILS=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DLLVM_USE_LINKER=mold \
    -DCMAKE_CXX_STANDARD=17 \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt;libunwind"
  echo "==== config llvm-project done ===="

install_latest_llvm:
  #!/usr/bin/env bash
  echo "==== build newest llvm ===="
  cd $HOME/projects/dev/cpp/llvm-project/build
  cmake --build . -j8
  sudo cmake --install $HOME/projects/dev/cpp/llvm-project/build
  sudo ln -s /usr/local/opt/llvm@18 /usr/local/opt/llvm
  echo "==== build newest llvm done ===="

config_llvm_for_triton:
  #!/usr/bin/env bash
  echo "==== config llvm-project ===="
  cd $HOME/projects/dev/cpp/llvm-triton/
  # git pull
  git checkout 6607f62b89e4
  trash-put build
    # -DCLANG_DEFAULT_CXX_STDLIB=libc++ \
  cmake -G Ninja -B build ./llvm \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_CXX_COMPILER=/usr/local/opt/llvm@18/bin/clang++ \
    -DCMAKE_C_COMPILER=/usr/local/opt/llvm@18/bin/clang \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local/opt/llvm-triton \
    -DMLIR_ENABLE_CUDA_RUNNER=ON \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
    -DLLVM_TARGETS_TO_BUILD="X86;NVPTX;RISCV;AMDGPU" \
    -DLLVM_ENABLE_PROJECTS="clang;flang;llvm;mlir;clang-tools-extra;lldb" \
    -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
    -DLLVM_LIT_ARGS=-v \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_INSTALL_UTILS=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DLLVM_USE_LINKER=mold \
    -DCMAKE_CXX_STANDARD=17 \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt;libunwind"
        # -DLLVM_ENABLE_RTTI=ON \
  echo "==== config llvm-project done ===="

install_llvm_for_triton:
  #!/usr/bin/env bash
  echo "==== build newest llvm ===="
  cd $HOME/projects/dev/cpp/llvm-triton/build
  cmake --build . -j8
  sudo cmake --install $HOME/projects/dev/cpp/llvm-project/build
  echo "==== build newest llvm done ===="

config_and_install_llvm_for_triton: config_llvm_for_triton install_llvm_for_triton

config_and_install_latest_llvm: config_latest_llvm install_latest_llvm

package_emacs:
  #!/usr/bin/env bash
  cd ~
  zip -r emacs.d.zip .emacs.d -x .emacs.d/.local/cache/eln/\* .emacs.d/.local/straight/build-30.0.50/\* .emacs.d/.local/straight/build.30.0.50.el .emacs.d/.local/cache/projectile.cache .emacs.d/.local/cache/projectile.projects .emacs.d/.local/cache/recentf .emacs.d/.local/cache/savehist .emacs.d/eln-cache/\*
  mv ~/emacs.d.zip ~/nutstore_files/Work/emacs.d.zip

config_cuda_play:
  #!/usr/bin/env bash
  echo "==== config CUDA play ===="
  cd $HOME/projects/dev/cpp/cuda_play
  trash-put build
  mkdir -p build
  cd build
  cmake -G "Ninja" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_CXX_STANDARD=23 \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" ../
  echo "==== config CUDA play done ===="

harfbuzz:
  #!/usr/bin/env bash
  echo "==== config harfbuzz ===="
  cd $HOME/projects/dev/cpp/harfbuzz
  trash-put build
  mkdir -p build
  cd build
  meson setup build -D graphite=enabled -D webassembly=enabled
  meson test -C build
  echo "==== config harfbuzz done ===="

install_local_llvm:
  #!/usr/bin/env bash
  echo "==== build local llvm ===="
  cd ~/projects/dev/cpp/llvm-vr/
  # git pull
  trash-put build
  mkdir -p build
  cd build
  cmake ../llvm -G "Ninja" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_INSTALL_PREFIX=/usr/local/opt/llvm@16 \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_TARGETS_TO_BUILD="X86;NVPTX;RISCV;AMDGPU" \
    -DLLVM_INSTALL_UTILS=ON \
    -DMLIR_ENABLE_CUDA_RUNNER=ON \
    -DLLVM_USE_LINKER=mold \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
    -DLLVM_ENABLE_PROJECTS="clang;mlir" \
    -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
    -DLLVM_LIT_ARGS=-v \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt"
  cmake --build .
  # trash-put $HOME/sdk/lib/llvm/*
  sudo cmake --install $HOME/projects/dev/cpp/llvm-vr/build
  echo "==== build local llvm done ===="

update:
  #!/usr/bin/env bash
  rustup update
  elan update
  cs update
  opam update
  opam upgrade
  nix-channel --update
  nix-env -u
  cabal update

triton:
  #!/usr/bin/env bash
  export LLVM_ROOT_DIR=/usr/local/opt/llvm-triton
  cd $HOME/projects/dev/cpp/triton
  mkdir -p build
  cd build
  export MLIR_DIR=$LLVM_ROOT_DIR/lib/cmake/mlir
  export LLVM_DIR=$LLVM_ROOT_DIR/lib/cmake/llvm
  cmake ../ -G Ninja \
  -DMLIR_DIR=$MLIR_DIR \
  -DCMAKE_CXX_COMPILER=/usr/local/opt/llvm-triton/bin/clang++ \
  -DCMAKE_C_COMPILER=/usr/local/opt/llvm-triton/bin/clang \
  -DLLVM_DIR=$LLVM_DIR \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DTRITON_BUILD_PYTHON_MODULE=ON \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
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

build_emacs_packages:
  #!/usr/bin/env bash
  trash-put $HOME/.config/.emacs.d/.local/straight/build-*
  trash-put $HOME/.config/.emacs.d/.local/autoloads*
  trash-put $HOME/.config/.emacs.d/eln-cache
  trash-put $HOME/.config/.emacs.d/.local/cache/eln
  trash-put $HOME/.config/.emacs.d/.local/etc/@
  trash-put $HOME/.config/.emacs.d/.local/straight/repos/org
  $HOME/.config/.emacs.d/bin/doom sync

pull:
  #!/usr/bin/env bash
  cd $HOME/projects/dev/rust-projects/Ambient
  git pull
  cd $HOME/projects/dev/rust-projects/wgpu
  git pull
  cd $HOME/projects/dev/rust-projects/rust-analyzer
  git pull
  cd $HOME/projects/dev/rust-projects/rust
  git pull --recursive-submodules
  cd $HOME/projects/dev/bevy
  git pull
  cd $HOME/projects/dev/rust-projects/naga
  git pull
  cd $HOME/projects/dev/rust-projects/naga_oil
  git pull
  cd $HOME/projects/dev/rust-projects/burn
  git pull
  cd $HOME/projects/dev/rust-projects/wasmer
  git pull
  cd $HOME/projects/dev/rust-projects/wasmtime
  git pull
  cd $HOME/projects/dev/rust-projects/iced
  git pull
  cd $HOME/projects/dev/rust-projects/servo
  git pull
  cd $HOME/projects/dev/rust-projects/rune
  git pull
  cd $HOME/projects/dev/rust-projects/winit
  git pull
  cd $HOME/projects/dev/cpp/llvm-project
  git pull
  cd $HOME/projects/dev/rust-projects/redox
  make pull

yosys:
  #!/usr/bin/env bash
  cd $HOME/projects/dev/cpp/yosys
  git pull
  bear -- make -j12

build_local_emacs:
  #!/usr/bin/env bash
  cd $HOME/projects/aur/emacs-pgtk-git/src/emacs-git
  git clean -fdx
  make bootstrap-clean
  make clean
  cd $HOME/projects/aur/emacs-pgtk-git/
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
  export TAICHI_CMAKE_ARGS="-DCMAKE_CXX_COMPILER=${CLANG_PATH}/bin/clang++  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON $TAICHI_CMAKE_ARGS"
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
  -DCMAKE_BUILD_TYPE=Debug ../

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
  echo "==== pull ROCm-Device-Libs done ===="

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
  mkdir -p build
  meson setup --reconfigure -Dgallium-rusticl=true -Dopencl-spirv=true -Dshader-cache=true -Dllvm=true ./build
  meson compile -C build
  echo "==== pull mesa done ===="

mpv:
  #!/usr/bin/env bash
  echo "==== pull mpv ===="
  cd ~/projects/dev/c-projects/mpv
  git pull
  mkdir -p build
  meson setup --reconfigure build
  meson compile -C build
  echo "==== pull mpv done ===="

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
  meson setup build
  echo "==== pull wlroots done ===="

mutter:
  #!/usr/bin/env bash
  echo "==== pull mutter ===="
  cd ~/projects/dev/c/mutter
  git pull
  meson setup build
  echo "==== pull mutter done ===="

vlc:
  #!/usr/bin/env bash
  echo "==== config vlc ===="
  cd ~/projects/dev/c/vlc
  git pull
  meson setup build
  echo "==== config vlc done ===="

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
  svn checkout https://svn.blender.org/svnroot/bf-blender/trunk/lib/linux_x86_64_glibc_228
  cd ~/projects/dev/c/blender-git/blender
  make update
  make ccache debug ninja
  mkdir -p ../build
  cd ../build
  #CC=clang CXX=clang++
  #-DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
  #CFLAGS="-fopenmp" CXXFLAGS="-fopenmp"
  cmake -G "Ninja" \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_BUILD_TYPE=Debug \
  -DWITH_LINKER_MOLD=ON ../blender
  #ninja -j12
  #developer ccache
  cp ./compile_commands.json ../blender
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

servo:
  #!/usr/bin/env bash
  echo "==== pull servo ===="
  cd ~/projects/dev/rust-projects/servo
  git pull
  echo "==== pull servo done ===="

egui:
  #!/usr/bin/env bash
  echo "==== pull egui ===="
  cd ~/projects/dev/rust-projects/egui
  git pull
  echo "==== pull egui done ===="

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
  git clean -fdx
  git pull
  LANG=en_US.UTF8 make 1c
  chmod 0600 /home/vitalyr/nutstore_files/Books/计算机科学/计算机底层/perfbook-1c.pdf
  cp perfbook-1c.pdf ~/nutstore_files/Books/计算机科学/计算机底层/perfbook-1c.pdf
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

verilator:
  #!/usr/bin/env bash
  echo "==== pull verilator ===="
  cd ~/projects/dev/cpp/verilator
  git pull
  autoconf
  ./configure
  bear -- make -j12 objdir=./build
  echo "==== pull verilator done ===="

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

the-art-of-hpc-book:
  #!/usr/bin/env bash
  echo "==== pull the art of hpc book ===="
  cd ~/nutstore_files/Books/计算机科学/高性能计算/TheArtofHPC_pdfs
  git pull
  echo "==== pull the art of hpc book done ===="


book: perfbook chisel-book algoxy-book eoc the-art-of-hpc-book
