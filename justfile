bt := '0'

export RUST_BACKTRACE := bt
export LLVM_INSTALL_DIR := "/home/vitalyr/sdk/lib/llvm"

log := "warn"

set shell := ["fish", "-c"]

export JUST_LOG := log

all: qemu linux servo mpv llvm-emacs-tools mold taichi ghc blender godot rust bevy book chisel-book rocm ra wgpu wasmtime wlroots mutter riscv-gnu riscv-isa-sim emacs agda agda-stdlib org verilator yosys egui

llvm-emacs-tools:
  #!/usr/bin/env bash
  echo "==== pull llvm-project ===="
  cd ~/projects/dev/cpp/llvm-project
  git pull
  cd ~/projects/dev/emacs-projects/llvm-tools
  cp ~/projects/dev/cpp/llvm-project/llvm/utils/emacs/*.el ./
  cp ~/projects/dev/cpp/llvm-project/mlir/utils/emacs/*.el ./
  git add -A
  # git commit -m "up"
  # git push
  echo "==== pull llvm-project done ===="

lean:
  #!/usr/bin/env bash
  #git clone https://github.com/leanprover/lean4 --recurse-submodules
  #git config submodule.recurse true
  export LEAN_SRC_PATH=$HOME/projects/dev/lean/lean4
  cd $LEAN_SRC_PATH
  git pull
  trash-put build
  cmake ./ -B build \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_EXE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_MODULE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_SHARED_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo
  make -C build -j$(nproc)

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

config_pytorch:
  #!/usr/bin/env bash
  export PYTORCH_SRC=$HOME/projects/dev/cpp/pytorch
  cd $PYTORCH_SRC
  # rm -rf build
  rm build/CMakeCache.txt
  git restore cmake/MiscCheck.cmake
  git restore CMakeLists.txt
  git submodule update --init --recursive
  git pull --recurse-submodules
  # Fix building against glog 0.7
  # patch -p1 -i "$XDG_CONFIG_HOME/patches/glog-0.7.patch"
  patch -Np1 -i "$XDG_CONFIG_HOME/patches/87773.patch"
  patch -Np1 -i "$XDG_CONFIG_HOME/patches/pytorch-missing-iostream.patch"
  patch -Np1 -i "$XDG_CONFIG_HOME/patches/pytorch-remove-caffe2-binaries.patch"
  # Disable -Werror
  patch -Np1 -d third_party/fbgemm -i "$XDG_CONFIG_HOME/patches/disable-werror1.patch"
  patch -Np1 -d third_party/benchmark -i "$XDG_CONFIG_HOME/patches/disable-werror2.patch"
  patch -Np1 -i "$XDG_CONFIG_HOME/patches/disable-werror4.patch"
  # https://bugs.archlinux.org/task/64981
  patch -N torch/utils/cpp_extension.py "$XDG_CONFIG_HOME/patches/fix_include_system.patch"
  export VERBOSE=1
  export PYTORCH_BUILD_VERSION="5.0.0rc14"
  export PYTORCH_BUILD_NUMBER=1
  # Check tools/setup_helpers/cmake.py, setup.py and CMakeLists.txt for a list of flags that can be set via env vars.
  export ATEN_NO_TEST=ON  # do not build ATen tests
  export USE_MKLDNN=ON
  export BUILD_CUSTOM_PROTOBUF=OFF
  # Caffe2 support was removed from pytorch with version 2.2.0
  export BUILD_CAFFE2=OFF
  export BUILD_CAFFE2_OPS=OFF
  export BUILD_SHARED_LIBS=OFF
  export USE_FFMPEG=ON
  export USE_GFLAGS=ON
  export USE_GLOG=ON
  export USE_VULKAN=ON
  export BUILD_BINARY=OFF
  export USE_DISTRIBUTED=0
  export USE_OBSERVERS=ON
  export USE_OPENCV=ON
  export USE_FBGEMM=1
  # export USE_SYSTEM_LIBS=ON  # experimental, not all libs present in repos
  export USE_SYSTEM_NCCL=ON
  export NCCL_VERSION=$(pkg-config nccl --modversion)
  export NCCL_VER_CODE=$(sed -n 's/^#define NCCL_VERSION_CODE\s*\(.*\).*/\1/p' /usr/include/nccl.h)
  # export BUILD_SPLIT_CUDA=ON  # modern preferred build, but splits libs and symbols, ABI break
  export USE_FAST_NVCC=0  # parallel build with nvcc, spawns too many processes
  export USE_CUPTI_SO=ON  # make sure cupti.so is used as shared lib
  export TORCH_SHOW_CPP_STACKTRACES=1
  export MAX_JOBS=12
  export CC=/usr/bin/clang
  export CXX=/usr/bin/clang++
  # export CFLAGS+=" -fuse-ld=mold"
  # export CXXFLAGS+=" -fuse-ld=mold"
  export LD=mold
  # export CAFFE2_STATIC_LINK_CUDA=1
  export LDFLAGS="-Wl,--as-needed"
  export BUILD_TEST=1
  export NVCC_CCBIN=/usr/bin/clang++
  export CUDAHOSTCXX="${NVCC_CCBIN}"
  export CUDA_HOST_COMPILER="${CUDAHOSTCXX}"
  export CUDA_HOME=/opt/cuda
  # hide build-time CUDA devices
  export CUDA_VISIBLE_DEVICES=""
  export CUDNN_LIB_DIR=/usr/lib
  export CUDNN_INCLUDE_DIR=/usr/include
  export TORCH_NVCC_FLAGS="-Xfatbin -compress-all"
  # CUDA arch 8.7 is not supported (needed by Jetson boards, etc.)
  export TORCH_CUDA_ARCH_LIST="5.2;5.3;6.0;6.1;6.2;7.0;7.2;7.5;8.0;8.6;8.9;9.0;9.0+PTX"  #include latest PTX for future compat
  export OVERRIDE_TORCH_CUDA_ARCH_LIST="${TORCH_CUDA_ARCH_LIST}"
  export CMAKE_C_COMPILER_LAUNCHER=sccache
  export CMAKE_CXX_COMPILER_LAUNCHER=sccache
  export CMAKE_CUDA_COMPILER_LAUNCHER=sccache
  export ROCM_PATH=/opt/rocm
  export HIP_ROOT_DIR=/opt/rocm
  export PYTORCH_ROCM_ARCH="gfx906;gfx908;gfx90a;gfx940;gfx941;gfx942;gfx1010;gfx1012;gfx1030;gfx1100;gfx1101;gfx1102"
  # Compile source code for supported GPU archs in parallel
  export HIPCC_COMPILE_FLAGS_APPEND="-parallel-jobs=$(nproc)"
  export HIPCC_LINK_FLAGS_APPEND="-parallel-jobs=$(nproc)"
  echo "Building pytorch with cuda and with non-x86-64 optimizations"
  export USE_CUDA=1
  export USE_CUDNN=1
  export USE_ROCM=0
  export MAGMA_HOME=/opt/cuda/targets/x86_64-linux
  echo "add_definitions(-march=x86-64)" >> cmake/MiscCheck.cmake
  # python setup.py develop --cmake
  # same horrible hack as above
  (USE_PRECOMPILED_HEADERS=1 python setup.py develop || python setup.py develop) && pip uninstall torch -y && python setup.py develop
  # do this hack when encountering issues like
  # Traceback (most recent call last):
  #   File "/home/vitalyr/projects/dev/cpp/triton/python/tutorials/01-vector-add.py", line 21, in <module>
  #     import torch
  #   File "/opt/miniconda3/envs/py3.11/lib/python3.11/site-packages/torch/__init__.py", line 237, in <module>
  #     from torch._C import *  # noqa: F403
  #     ^^^^^^^^^^^^^^^^^^^^^^
  # ImportError: /opt/miniconda3/envs/py3.11/bin/../lib/libgcc_s.so.1: version `GCC_12.0.0' not found (required by /usr/lib/libQt6Test.so.6)
  #
  # cp /usr/lib/libgcc_s.so.1 /opt/miniconda3/envs/py3.11/lib/
  echo "Building pytorch with cuda done"

zed:
  #!/usr/bin/env bash
  echo "==== config zed ===="
  export RUSTUP_TOOLCHAIN=stable
  export CARGO_TARGET_DIR=target
  export CFLAGS+=' -ffat-lto-objects'
  export CXXFLAGS+=' -ffat-lto-objects'
  export ZED_SRC_PATH=$HOME/projects/dev/rust-projects/zed
  cd $ZED_SRC_PATH
  git pull --recurse-submodules
  declare -gA _tags=([protocol]="8645a138fb2ea72c4dab13e739b1f3c9ea29ac84")
  axel "https://github.com/livekit/protocol/archive/${_tags[protocol]}/protocol-${_tags[protocol]}.tar.gz"
  rm -r crates/live_kit_server/protocol
  ln -sT "protocol-${_tags[protocol]}" crates/live_kit_server/protocol
  # cargo fetch --locked --target "$(rustc -vV | sed -n 's/host: //p')"
  gendesk -q -f -n \
   --name 'Zed' \
   --exec 'Zed' \
   --pkgname 'zed-editor-git' \
   --categories 'Office'
  cargo build --release --all-features
  cargo test --all-features
  echo "==== config zed done ===="

llama:
  #!/usr/bin/env bash
  echo "==== config llama.cpp ===="
  export LLAMA_CPP_SRC_PATH=$HOME/projects/dev/cpp/llama.cpp
  cd $LLAMA_CPP_SRC_PATH
  git checkout master
  git pull
  trash-put build
  cmake -G Ninja -B build ./ \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH"
  cd build
  cmake --build . --config Release
  echo "==== config llama.cpp ===="

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
  rm build/CMakeCache.txt
  rm build/NATIVE/CMakeCache.txt
  cmake -G Ninja -B build ./llvm \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER=/usr/bin/clang++ \
    -DCMAKE_C_COMPILER=/usr/bin/clang \
    -DCMAKE_EXE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_MODULE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_SHARED_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_USE_LINKER=mold \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local/opt/llvm@latest \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
    -DLLVM_TARGETS_TO_BUILD="X86;NVPTX;RISCV;AMDGPU" \
    -DLLVM_ENABLE_PROJECTS="clang;flang;llvm;mlir;clang-tools-extra;lldb;pstl;bolt" \
    -DLLVM_ENABLE_RUNTIMES="openmp;compiler-rt;libcxx;libc;libcxxabi;libunwind;offload" \
    -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
    -DMLIR_ENABLE_CUDA_RUNNER=1 \
    -DMLIR_ENABLE_SYCL_RUNNER=1 \
    -DMLIR_ENABLE_VULKAN_RUNNER=1 \
    -DMLIR_ENABLE_SPIRV_CPU_RUNNER=1 \
    -DMLIR_INCLUDE_INTEGRATION_TESTS=1 \
    -DMLIR_ENABLE_CUDA_CUSPARSE=1 \
    -DMLIR_RUN_CUDA_TENSOR_CORE_TESTS=1 \
    -DLLVM_LIT_ARGS=-v \
    -DLLVM_HAS_NVPTX_TARGET=1 \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_BUILD_UTILS=ON \
    -DLLVM_BUILD_TOOLS=ON \
    -DLLVM_INSTALL_UTILS=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DCMAKE_CXX_STANDARD=17
    # -DMLIR_ENABLE_CUDA_CUSPARSELT=1 \
  echo "==== config llvm-project done ===="

install_latest_llvm:
  #!/usr/bin/env bash
  echo "==== build newest llvm ===="
  cd $HOME/projects/dev/cpp/llvm-project/build
  cmake --build . -j10
  cmake --install $HOME/projects/dev/cpp/llvm-project/build
  ln -s /usr/local/opt/llvm@latest /usr/local/opt/llvm
  echo "==== build newest llvm done ===="

config_llvm_19:
  #!/usr/bin/env bash
  echo "==== config llvm-project ===="
  cd $HOME/projects/dev/cpp/llvm-vr
  git co -b release-19.x origin/release/19.x
  git co release-19.x
  git pull
  rm build/CMakeCache.txt
  rm build/NATIVE/CMakeCache.txt
  cmake -G Ninja -B build ./llvm \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_COMPILER=clang \
    -DLLVM_CCACHE_BUILD=ON \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local/opt/llvm@19 \
    -DMLIR_ENABLE_CUDA_RUNNER=ON \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
    -DLLVM_TARGETS_TO_BUILD="X86;NVPTX;RISCV;AMDGPU" \
    -DLLVM_ENABLE_PROJECTS="clang;flang;llvm;mlir;clang-tools-extra;lldb;pstl;bolt" \
    -DLLVM_ENABLE_RUNTIMES="openmp;compiler-rt;libcxx;libc;libcxxabi;libunwind;offload" \
    -DMLIR_ENABLE_BINDINGS_PYTHON="ON" \
    -DMLIR_ENABLE_CUDA_RUNNER=1 \
    -DMLIR_ENABLE_SYCL_RUNNER=1 \
    -DMLIR_ENABLE_VULKAN_RUNNER=1 \
    -DMLIR_ENABLE_SPIRV_CPU_RUNNER=1 \
    -DLLVM_LIT_ARGS=-v \
    -DLLVM_HAS_NVPTX_TARGET=1 \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_BUILD_UTILS=ON \
    -DLLVM_BUILD_TOOLS=ON \
    -DLLVM_INSTALL_UTILS=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DLLVM_USE_LINKER=mold \
    -DCMAKE_CXX_STANDARD=17
  echo "==== config llvm-project done ===="

install_llvm_19:
  #!/usr/bin/env bash
  echo "==== build llvm 19 ===="
  cd $HOME/projects/dev/cpp/llvm-vr/build
  cmake --build . -j10
  cmake --install $HOME/projects/dev/cpp/llvm-vr/build
  echo "==== build llvm 19 done ===="

llvm_19: config_llvm_19 install_llvm_19

xla:
  #!/usr/bin/env bash
  echo "==== config xla ===="
  export XLA_SRC_PATH=$HOME/projects/dev/cpp/xla
  cd $XLA_SRC_PATH
  git checkout main
  git pull
  ./configure.py --backend=CUDA --host_compiler=clang --nccl --clang_path=/usr/local/opt/llvm@17/bin/clang --gcc_path=/usr/bin/gcc
  bazel aquery "mnemonic(CppCompile, //xla/...)" --output=jsonproto | python3 build_tools/lint/generate_compile_commands.py
  bazel build --test_output=all //xla/... --experimental_repo_remote_exec --config=monolithic
  echo "==== config xla done ===="

stablehlo:
  #!/usr/bin/env bash
  echo "==== config stablehlo ===="
  export STABLEHLO_SRC_PATH=$HOME/projects/dev/cpp/stablehlo
  cd $STABLEHLO_SRC_PATH
  cd llvm-project && git fetch && git checkout $(cat ../build_tools/llvm_version.txt)
  cd ..
  pip install -r ./llvm-project/mlir/python/requirements.txt
  MLIR_ENABLE_BINDINGS_PYTHON=ON build_tools/build_mlir.sh $PWD/llvm-project/ $PWD/llvm-build
  mkdir -p build && cd build
  cmake .. -G Ninja \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DCMAKE_INSTALL_PREFIX=/usr/local/opt/stablehlo \
    -DSTABLEHLO_ENABLE_BINDINGS_PYTHON=OFF \
    -DSTABLEHLO_ENABLE_SPLIT_DWARF=ON \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_C_COMPILER=clang \
    -DLLVM_EXTERNAL_LIT=/usr/bin/lit \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_EXE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_MODULE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_SHARED_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DSTABLEHLO_ENABLE_SANITIZER=address \
    -DMLIR_DIR=$PWD/../llvm-build/lib/cmake/mlir
    # -DSTABLEHLO_ENABLE_LLD=ON \
  cmake --build .
  cmake --install $STABLEHLO_SRC_PATH/build
  ninja check-stablehlo-tests
  # cd ..
  # STABLEHLO_ENABLE_BINDINGS_PYTHON=ON ./build_tools/github_actions/ci_build_cmake.sh $PWD/llvm-build $PWD/build

iree:
  #!/usr/bin/env bash
  echo "==== config iree ===="
  export IREE_SRC_PATH=$HOME/projects/dev/cpp/iree
  cd $IREE_SRC_PATH
  git checkout main
  git submodule update --init
  git pull --recurse-submodules
  rm build/CMakeCache.txt
  rm build/NATIVE/CMakeCache.txt
  # Recommended development options using clang and mold:
  # Use conda environment before this command!
  conda activate py3.11
  proxychains -q pip install -r runtime/bindings/python/iree/runtime/build_requirements.txt
  cmake -G Ninja -B build -S . \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DIREE_ENABLE_WERROR_FLAG=OFF \
    -DIREE_ENABLE_ASSERTIONS=ON \
    -DIREE_ENABLE_SPLIT_DWARF=ON \
    -DIREE_ENABLE_RUNTIME_TRACING=ON \
    -DIREE_ENABLE_THIN_ARCHIVES=ON \
    -DIREE_HAL_DRIVER_CUDA=ON \
    -DIREE_HAL_DRIVER_VULKAN=ON \
    -DIREE_TARGET_BACKEND_DEFAULTS=ON \
    -DIREE_INPUT_STABLEHLO=ON \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DIREE_BUILD_PYTHON_BINDINGS=ON  \
    -DPython3_EXECUTABLE="$(which python)" \
    -DCMAKE_EXE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_MODULE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_SHARED_LINKER_FLAGS_INIT="-fuse-ld=mold"
  cmake --build build
  cmake --build build --target iree-test-deps
  ctest -R build/tests/e2e/linalg/conv2d.mlir
  echo "==== config iree done ===="

config_llvm_for_triton:
  #!/usr/bin/env bash
  echo "==== config llvm-project for triton ===="
  export TRITON_SRC_PATH=$HOME/projects/dev/cpp/triton
  cd $TRITON_SRC_PATH
  git checkout main
  git pull
  cd $HOME/projects/dev/cpp/llvm-triton/
  git checkout main
  git pull
  git checkout $(cat $TRITON_SRC_PATH/cmake/llvm-hash.txt)
  rm build/CMakeCache.txt
  rm build/NATIVE/CMakeCache.txt
  cmake -G Ninja -B build ./llvm \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_COMPILER=clang \
    -DLLVM_USE_LINKER=mold \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_ENABLE_TERMINFO=OFF \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local/opt/llvm-triton \
    -DMLIR_ENABLE_CUDA_RUNNER=ON \
    -DLLVM_TARGETS_TO_BUILD="X86;NVPTX;AMDGPU" \
    -DLLVM_ENABLE_PROJECTS="mlir" \
    -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
    -DLLVM_LIT_ARGS=-v \
    -DMLIR_ENABLE_CUDA_RUNNER=ON \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_BUILD_UTILS=ON \
    -DLLVM_BUILD_TOOLS=ON \
    -DLLVM_INSTALL_UTILS=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DCMAKE_CXX_STANDARD=17
  echo "==== config llvm-project for triton done ===="

install_llvm_for_triton:
  #!/usr/bin/env bash
  echo "==== build llvm for triton ===="
  cd $HOME/projects/dev/cpp/llvm-triton/build
  cmake --build . -j20
  cmake --install $HOME/projects/dev/cpp/llvm-triton/build
  echo "==== build llvm for triton done ===="

config_and_install_llvm_for_triton: config_llvm_for_triton install_llvm_for_triton

llvm_latest: config_latest_llvm install_latest_llvm

package_emacs:
  #!/usr/bin/env bash
  cd ~
  zip -r emacs.d.zip .emacs.d -x .emacs.d/.local/cache/eln/\* .emacs.d/.local/straight/build-30.0.50/\* .emacs.d/.local/straight/build.30.0.50.el .emacs.d/.local/cache/projectile.cache .emacs.d/.local/cache/projectile.projects .emacs.d/.local/cache/recentf .emacs.d/.local/cache/savehist .emacs.d/eln-cache/\*
  mv ~/emacs.d.zip ~/nutstore_files/Work/emacs.d.zip

cuda_play:
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
  time ninja all -j12
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
    -DCMAKE_INSTALL_PREFIX=/usr/local/opt/llvm@17 \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_TARGETS_TO_BUILD="X86;NVPTX;RISCV;AMDGPU" \
    -DLLVM_INSTALL_UTILS=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DLLVM_INSTALL_UTILS=ON     \
    -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
    -DLLVM_ENABLE_PROJECTS="clang;mlir" \
    -DMLIR_ENABLE_BINDINGS_PYTHON=ON \
    -DLLVM_LIT_ARGS=-v \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=mold" \
    -DCMAKE_MODULE_LINKER_FLAGS="-fuse-ld=mold" \
    -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=mold" \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt"
  cmake --build .
  # trash-put $HOME/sdk/lib/llvm/*
  cmake --install $HOME/projects/dev/cpp/llvm-vr/build
  echo "==== build local llvm done ===="

zig:
  #!/usr/bin/env bash
  echo "==== build local zig ===="
  cd ~/projects/dev/zig/zig
  git pull
  trash-put build
  mkdir -p build
  cd build
  cmake ../ -G "Ninja" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_INSTALL_PREFIX=/usr/local/opt/zig \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_COMPILER=clang
    # -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld" \
    # -DCMAKE_MODULE_LINKER_FLAGS="-fuse-ld=lld" \
    # -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=lld"
  cmake --build .
  # trash-put $HOME/sdk/lib/llvm/*
  cmake --install $HOME/projects/dev/zig/zig/build
  echo "==== build local zig done ===="

update:
  #!/usr/bin/env bash
  rustup update
  elan toolchain install nightly
  cs update
  opam update
  opam upgrade
  nix-channel --update
  nix-env -u
  cabal update

duckdb:
  #!/usr/bin/env bash
  export DUCKDB_SRC_PATH=$HOME/projects/dev/cpp/duckdb
  cd $DUCKDB_SRC_PATH
  rm build/relassert/CMakeCache.txt
  git pull
  CC=/usr/bin/clang CXX=/usr/bin/clang++ BUILD_JDBC=1 BUILD_ODBC=1 BUILD_SHELL=1 BUILD_PYTHON=1  GEN=ninja make relassert

kvrocks:
  #!/usr/bin/env bash
  export KVROCKS_SRC_PATH=$HOME/projects/dev/cpp/kvrocks
  cd $KVROCKS_SRC_PATH
  git pull
  mkdir -p build
  rm build/CMakeCache.txt
  ./x.py build -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -j12 \
      -DCMAKE_C_COMPILER_LAUNCHER=sccache \
      -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
      -DCMAKE_CXX_COMPILER=clang++ \
      -DCMAKE_C_COMPILER=clang \
      -DCMAKE_EXE_LINKER_FLAGS_INIT="-latomic_ops -fuse-ld=mold" \
      -DCMAKE_MODULE_LINKER_FLAGS_INIT="-latomic_ops -fuse-ld=mold" \
      -DCMAKE_SHARED_LINKER_FLAGS_INIT="-latomic_ops -fuse-ld=mold"
  
cutlass:
  #!/usr/bin/env bash
  export CUTLASS_SRC_PATH=$HOME/projects/dev/cpp/cutlass
  cd $CUTLASS_SRC_PATH
  git pull
  mkdir -p build
  rm build/CMakeCache.txt
  cd build
  # build for NVIDIA Ampere GPU Architecture
  cmake ../ -G Ninja -DCUTLASS_NVCC_ARCHS=80 \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_EXE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_MODULE_LINKER_FLAGS_INIT="-fuse-ld=mold" \
    -DCMAKE_SHARED_LINKER_FLAGS_INIT="-fuse-ld=mold"
  cmake --build .
  
comfyui:
  #!/usr/bin/env bash
  export COMFYUI_SRC_PATH=$HOME/projects/dev/ai/ComfyUI
  cd $COMFYUI_SRC_PATH
  git pull

typst:
  #!/usr/bin/env bash
  export TYPST_SRC_PATH=$HOME/projects/dev/rust-projects/typst
  cd $TYPST_SRC_PATH
  git pull

cpython:
  #!/usr/bin/env bash
  export CPYTHON_SRC_PATH=$HOME/projects/dev/cpp/cpython
  cd $CPYTHON_SRC_PATH
  git pull
  CC=clang CXX=clang++ ./configure --with-pydebug --disable-gil
  make -j10

jax:
  #!/usr/bin/env bash
  export JAX_SRC_PATH=$HOME/projects/dev/cpp/jax
  export XLA_SRC_PATH=$HOME/projects/dev/cpp/xla
  cd $JAX_SRC_PATH
  trash-put dist
  git pull
  # bazel run @hedron_compile_commands//:refresh_all
  #-- --config=cuda --config=cuda_plugin --config=nvcc_clang
  python build/build.py --enable_cuda --build_gpu_plugin --gpu_plugin_cuda_version=12 --use_clang --clang_path /usr/bin/clang
  # --bazel_options=--override_repository=xla=$XLA_SRC_PATH
  pip install dist/*.whl --force-reinstall  # installs jaxlib (includes XLA)
  pip install -e .  --force-reinstall # installs jax

triton:
  #!/usr/bin/env bash
  export LLVM_ROOT_DIR=/usr/local/opt/llvm-triton
  export LLVM_BUILD_DIR=$HOME/projects/cpp/llvm-triton/build
  export TRITON_BUILD_WITH_CCACHE=true
  export LLVM_INCLUDE_DIRS=$LLVM_ROOT_DIR/include
  export LLVM_LIBRARY_DIR=$LLVM_ROOT_DIR/lib
  export LLVM_SYSPATH=$LLVM_ROOT_DIR
  export TRITON_SRC_PATH=$HOME/projects/dev/cpp/triton
  cd $TRITON_SRC_PATH
  # rm -rf build
  mkdir -p build
  cd build
  export MLIR_DIR=$LLVM_ROOT_DIR/lib/cmake/mlir
  export LLVM_DIR=$LLVM_ROOT_DIR/lib/cmake/llvm
  cmake ../ -G Ninja \
    -DMLIR_DIR=$MLIR_DIR \
    -DLLVM_DIR=$LLVM_DIR \
    -DLLVM_EXTERNAL_LIT=/usr/bin/lit  \
    -DCMAKE_BUILD_TYPE=TritonRelBuildWithAsserts \
    -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=mold" \
    -DCMAKE_MODULE_LINKER_FLAGS="-fuse-ld=mold" \
    -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=mold" \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_C_COMPILER_LAUNCHER=sccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
    -DTRITON_CODEGEN_BACKENDS="nvidia;amd" \
    -DCUPTI_INCLUDE_DIR="$TRITON_SRC_PATH/third_party/nvidia/backend/include" \
    -DROCTRACER_INCLUDE_DIR="$TRITON_SRC_PATH/third_party/amd/backend/include" \
    -DJSON_INCLUDE_DIR:UNINITIALIZED="$HOME/.triton/json/include" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DTRITON_BUILD_PYTHON_MODULE=ON    \
    -DLLVM_ENABLE_ASSERTIONS=ON

  cmake --build .
  # test
  # pip install -e '.tests'
  # pytest -vs test/unit

triton_and_llvm: config_and_install_llvm_for_triton triton

triton_wheel:
  #!/usr/bin/env bash
  export TRITON_BUILD_WITH_CLANG_LLD=1
  export LLVM_ROOT_DIR=/usr/local/opt/llvm-triton
  export LLVM_BUILD_DIR=$HOME/projects/cpp/llvm-triton/build
  export TRITON_BUILD_WITH_CCACHE=true
  export LLVM_INCLUDE_DIRS=$LLVM_ROOT_DIR/include
  export LLVM_LIBRARY_DIR=$LLVM_ROOT_DIR/lib
  export LLVM_SYSPATH=$LLVM_ROOT_DIR
  export TRITON_BUILD_PROTON=1
  export LD=mold
  # export DEBUG=1
  cd $HOME/projects/dev/cpp/triton
  # git clean -fdx
  rm build/CMakeCache.txt
  git pull
  cd python
  # use conda's py3.11 environment
  # run conda activate py3.11 first
  # source /opt/miniconda3/etc/fish/conf.d/conda.fish
  # conda activate py3.11
  python setup.py bdist_wheel
  # pip install torch
  # pip install --pre torch torchvision torchaudio --index-url https://download.pytorch.org/whl/nightly/cu124
  pip install numpy
  pip install pytest
  pip install tabulate
  pip install dist/triton-3.0.0-cp311-cp311-linux_x86_64.whl --force-reinstall

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
  # trash-put $HOME/.config/.emacs.d/.local/straight/repos/org
  trash-put $HOME/.config/.emacs.d/.local/straight/repos/build-31.0.50-cache.el
  trash-put $HOME/.config/.emacs.d/.local/straight/repos/build-31.0.50
  $HOME/.config/.emacs.d/bin/doom sync

pull: blender
  #!/usr/bin/env bash
  cd $HOME/projects/dev/rust-projects/egui
  git pull
  cd $HOME/projects/dev/cpp/triton
  git pull
  cd $HOME/projects/dev/cpp/pytorch
  git pull
  # cd $HOME/projects/dev/rust-projects/Ambient
  # git pull
  cd $HOME/projects/dev/rust-projects/avian
  git pull
  cd $HOME/projects/dev/rust-projects/bevy_editor_prototypes
  git pull
  cd $HOME/projects/dev/rust-projects/typst
  git pull
  cd $HOME/projects/dev/rust-projects/candle
  git pull
  cd $HOME/projects/dev/rust-projects/wgpu
  git pull
  cd $HOME/projects/dev/rust-projects/rust-analyzer
  git pull
  cd $HOME/projects/dev/rust-projects/rust
  git pull --recurse-submodules
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
  cd $HOME/projects/dev/cpp/godot
  git pull
  cd $HOME/projects/dev/c-projects/mpv
  git pull
  cd $HOME/projects/dev/cpp/mold
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
  # edit commit in the PKGBUILD
  mksrcinfo
  makepkg -si
  # proxychains -q $HOME/.config/.emacs.d/bin/doom upgrade --force

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
  git submodule update --init --recursive
  mkdir -p build
  # rm build/CMakeCache.txt
  cd build
  cmake -G "Ninja" \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_C_COMPILER_LAUNCHER=sccache \
  -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
  -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=mold" \
  -DCMAKE_MODULE_LINKER_FLAGS="-fuse-ld=mold" \
  -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=mold" \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo ../
  time cmake --build . -j10
 
build_circt:
  #!/usr/bin/env bash
  cd $HOME/projects/dev/cpp/circt/llvm/build
  # build llvm
  #ninja
  #ninja check-mlir
  cd $HOME/projects/dev/cpp/circt/build
  time ninja

circt:
  #!/usr/bin/env bash
  mkdir -p $HOME/projects/dev/cpp/circt/llvm/build
  cd $HOME/projects/dev/cpp/circt/
  git pull
  git submodule update --init
  mkdir -p llvm/build
  cd $HOME/projects/dev/cpp/circt/llvm/build
  git fetch --unshallow
  # config llvm
  cmake -G Ninja ../llvm \
  -DLLVM_ENABLE_PROJECTS="llvm;mlir" \
  -DLLVM_TARGETS_TO_BUILD="host" \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_C_COMPILER_LAUNCHER=sccache \
  -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
  -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=mold" \
  -DCMAKE_MODULE_LINKER_FLAGS="-fuse-ld=mold" \
  -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=mold" \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
  ninja -j12
  ninja check-mlir
  # build circt
  mkdir -p $HOME/projects/dev/cpp/circt/build
  cd $HOME/projects/dev/cpp/circt/build
  cmake -G Ninja .. \
  -DMLIR_DIR=$PWD/../llvm/build/lib/cmake/mlir \
  -DLLVM_DIR=$PWD/../llvm/build/lib/cmake/llvm \
  -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=mold" \
  -DCMAKE_MODULE_LINKER_FLAGS="-fuse-ld=mold" \
  -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=mold" \
  -DCMAKE_CXX_COMPILER=/usr/bin/clang++ \
  -DCMAKE_C_COMPILER=/usr/bin/clang \
  -DCMAKE_C_COMPILER_LAUNCHER=sccache \
  -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo \
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
#   -DLLVM_ENABLE_PROJECTS="clang;flang;llvm;mlir;clang-tools-extra"\
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
  time scons platform=linuxbsd -j 12 target=editor compiledb=true linker=mold debug_symbols=yes builtin_embree=no builtin_enet=no builtin_freetype=no builtin_graphite=no builtin_harfbuzz=no builtin_libogg=no builtin_libpng=no builtin_libtheora=no builtin_libvorbis=no builtin_libwebp=no builtin_mbedtls=no builtin_pcre2=no builtin_zlib=no builtin_zstd=no use_llvm=yes use_static_cpp=no
  #builtin_miniupnpc=no
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

qemu:
  #!/usr/bin/env bash
  echo "==== pull qemu ===="
  export QEMU_SRC_PATH=$HOME/projects/dev/c/qemu
  cd $QEMU_SRC_PATH
  git pull
  # mkdir build
  # cd build
  # ../configure --prefix=$RISCV
  # bear -- make -j12
  echo "==== pull qemu ===="

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
  -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_C_COMPILER_LAUNCHER=sccache \
  -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
  -DWITH_CLANG=ON \
  -DWITH_LINKER_MOLD=ON ../blender
  #ninja -j12
  #developer ccache
  cp ./compile_commands.json ../blender
  echo "==== pull blender done ===="

redox:
  #!/usr/bin/env bash
  echo "==== pull redox ===="
  cd ~/projects/dev/rust-projects/redox
  # make clean
  make pull
  make all
  echo "==== pull redox done ===="

rust:
  #!/usr/bin/env bash
  echo "==== pull rust ===="
  cd ~/projects/dev/rust-projects/rust
  git pull
  x build
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
  make CC=clang LD=ld.lld AR=llvm-ar NM=llvm-nm STRIP=llvm-strip \
    OBJCOPY=llvm-objcopy OBJDUMP=llvm-objdump OBJSIZE=llvm-size \
    READELF=llvm-readelf HOSTCC=clang HOSTCXX=clang++ HOSTAR=llvm-ar \
    HOSTLD=ld.lld
    # defconfig
  ./scripts/clang-tools/gen_compile_commands.py
  # use virtme-ng
  CC=clang HOSTCC=clang HOSTCXX=clang++ time vng --build --config .config
  echo "=== pull and build linux done ==="

ra:
  #!/usr/bin/env bash
  echo "==== pull rust-analyzer ===="
  cd ~/projects/dev/rust-projects/rust-analyzer
  git pull
  cargo build --release
  install -Dt /usr/local/bin target/release/rust-analyzer
  echo "==== pull rust-analyzer done ===="

servo:
  #!/usr/bin/env bash
  echo "==== pull servo ===="
  cd ~/projects/dev/rust-projects/servo
  git pull
  ./mach bootstrap
  ./mach build -d
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


book: perfbook algoxy-book eoc the-art-of-hpc-book chisel-book
