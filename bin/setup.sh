#!/bin/bash

dir="$(pwd)"
bin="$0"
while [ -L "$(dirname "${bin}")" ]
do
  bin="$(readlink "$(dirname "${bin}")")/$(basename "${bin}")"
done
while [ -L "${bin}" ]
do
    x="$(readlink "${bin}")"
    cd "$(dirname "${bin}")"
    bin="${x}"
done
cd "$(dirname "${bin}")"
home="$(dirname "$(pwd)")"

if [ ! -f "${home}/src/bcrypt/build/libbcrypt.so" ]
then
  if [ ! -f "${home}/src/bcrypt/build/libbcrypt.dylib" ]
  then
    set -ex
    cd "$home"
    make
    set +ex
  fi
fi

if [ ! -d "${home}/bin/racket/bin" ]
then
  mkdir -p "${home}/bin/racket/bin"
fi

if [ ! -x "${home}/bin/racket/bin/racket" ]
then
  _os="$(uname -s)"
  _arch="$(uname -m)"
  case "${_arch}" in
    arm64) _arch="aarch64" ;;
  esac
  case "${_os}" in
    Linux*)
      _platform="${_arch}-linux-cs"
      ;;
    Darwin*)
      _platform="${_arch}-macosx-cs"
      ;;
    *)
      echo "Don't know how to install racket on ${_os}!" 1>&2
      echo "Visit https://download.racket-lang.org/ and install racket to ${home}/bin/racket" 1>&2
      exit 1
      ;;
  esac
  _tarball="racket-minimal-9.1-${_platform}.tgz"
  set -ex
  cd "${home}/bin"
  wget "https://download.racket-lang.org/releases/9.1/installers/${_tarball}" -O "${_tarball}"
  tar xvf "${_tarball}"
  set +ex

  cd "${home}"
  "${home}/bin/racket/bin/raco" pkg install --auto compiler-lib
  "${home}/bin/racket/bin/raco" make *.scm
fi

cd "${dir}"

# source the .env file if it exists
if [ -f "${home}/.env" ]
then
  source "${home}/.env"
fi

export ARC_HOME="${home}"
export ARC_HOST="${ARC_HOST:-${RACKET:-${home}/bin/racket/bin/racket}}"
export DEV="${DEV:-1}"

