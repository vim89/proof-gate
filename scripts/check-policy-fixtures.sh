#!/usr/bin/env bash
set -euo pipefail

log_file="$(mktemp)"

if sbt "fixturesPolicyFail / scalafixAll --check" >"${log_file}" 2>&1; then
  cat "${log_file}"
  echo "Expected policy fixtures to fail, but Scalafix passed."
  rm -f "${log_file}"
  exit 1
fi

cat "${log_file}"

required_rules=(
  "NoSysEnv"
  "NoSystemGetenv"
  "NoConfigFactory"
  "NoKeywordTry"
  "NoKeywordCatch"
  "NoKeywordThrow"
)

for rule in "${required_rules[@]}"; do
  if ! grep -q "${rule}" "${log_file}"; then
    echo "Expected Scalafix output to include ${rule}."
    rm -f "${log_file}"
    exit 1
  fi
done

rm -f "${log_file}"
echo "Policy fixtures failed as expected."
