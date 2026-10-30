#!/bin/sh
# sync-qestats.sh — vendor libqe-stats' C++ headers (namespace qestats; rhoR's
#                   rho is qestats::rho) into inst/include/qestats/.
#
# The vendored copy is committed, so building rhoR never needs this script.
# Run it to move rhoR to a newer libqe-stats, then run the tests: the
# regression pins in tests/testthat/test-regression-pins.R must still pass.
#
#   from a libqe-stats checkout:   QESTATS_INCLUDE=<libqe-stats>/include sh tools/sync-qestats.sh
#   from the qe-libs Conan registry (released versions only):
#                                  QESTATS_VERSION=0.1.2 sh tools/sync-qestats.sh
#
# inst/include/qestats/SOURCE records where the headers came from.
#
# Conan: uses `conan` on PATH, else installs it into a throwaway venv with
# $PYTHON (default python3), with a throwaway CONAN_HOME.

set -e
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
QESTATS_VERSION="${QESTATS_VERSION:-0.1.2}"
CONAN_REMOTE="https://gitlab.com/api/v4/projects/22522458/packages/conan"
DST="${REPO_ROOT}/inst/include/qestats"

TMP="$(mktemp -d)"
trap 'rm -rf "${TMP}"' EXIT

if [ -n "${QESTATS_INCLUDE:-}" ]; then
    SRC="${QESTATS_INCLUDE}/qestats"
    [ -f "${SRC}/qestats.hpp" ] || { echo "ERROR: no qestats.hpp in ${SRC}" >&2; exit 1; }
    FROM="libqe-stats checkout"
    if GIT_REV="$(git -C "${QESTATS_INCLUDE}" rev-parse --short HEAD 2>/dev/null)"; then
        FROM="${FROM} at ${GIT_REV}"
        git -C "${QESTATS_INCLUDE}" diff --quiet HEAD -- . || FROM="${FROM} (with uncommitted changes)"
    fi
else
    if command -v conan >/dev/null 2>&1; then
        CONAN=conan
    else
        echo "Installing conan into a throwaway venv (${PYTHON:-python3})..."
        "${PYTHON:-python3}" -m venv "${TMP}/venv"
        VBIN="${TMP}/venv/bin"
        [ -d "${VBIN}" ] || VBIN="${TMP}/venv/Scripts"   # Windows venv layout
        "${VBIN}/python" -m pip install --quiet conan
        CONAN="${VBIN}/conan"
    fi
    export CONAN_HOME="${TMP}/conan"
    ${CONAN} remote add qe-libs "${CONAN_REMOTE}" --force >/dev/null
    echo "Downloading libqe-stats/${QESTATS_VERSION} from ${CONAN_REMOTE}..."
    ${CONAN} download "libqe-stats/${QESTATS_VERSION}:*" -r qe-libs >/dev/null 2>&1 || {
        echo "ERROR: libqe-stats/${QESTATS_VERSION} not found on ${CONAN_REMOTE}" >&2; exit 1; }
    SRC="$(find "${CONAN_HOME}/p" -type d -path "*/p/include/qestats" | head -1)"
    [ -n "${SRC}" ] || { echo "ERROR: no include/qestats in libqe-stats/${QESTATS_VERSION}" >&2; exit 1; }
    FROM="libqe-stats ${QESTATS_VERSION} (Conan)"
fi

# Clear the old copy so a header removed upstream is not left behind.
rm -rf "${DST}"
mkdir -p "${DST}"
cp "${SRC}"/*.hpp "${DST}/"
printf '%s\n' "${FROM}" > "${DST}/SOURCE"
echo "Synced ${FROM} -> inst/include/qestats"
