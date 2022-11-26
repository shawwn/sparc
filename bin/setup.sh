#!/bin/bash

dir="$(pwd)"
bin="$0"
while [ -L "${bin}" ]
do
    x="$(readlink "${bin}")"
    cd "$(dirname "${bin}")"
    bin="${x}"
done
cd "$(dirname "${bin}")"
home="$(dirname "$(pwd)")"
cd "${dir}"

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
  case "$(uname -s)" in
    Linux*)
      set -ex
      cd "${home}/bin"
      wget https://download.racket-lang.org/releases/8.7/installers/racket-minimal-8.7-x86_64-linux-cs.tgz
      tar xvf racket-minimal-8.7-x86_64-linux-cs.tgz
      set +ex
      ;;
    Darwin*)
      set -ex
      cd "${home}/bin"
      wget https://download.racket-lang.org/releases/8.7/installers/racket-minimal-8.7-aarch64-macosx-cs.tgz
      tar xvf racket-minimal-8.7-aarch64-macosx-cs.tgz
      set +ex
      ;;
    *)
      echo "Don't know how to install racket on $(uanme -s)!" 1>&2 
      echo "Visit https://download.racket-lang.org/ and install racket to ${home}/bin/racket" 1>&2
      exit 1
      ;;
  esac

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

