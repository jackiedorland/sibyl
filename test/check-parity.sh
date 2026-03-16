#!/usr/bin/env bash
# checks that every Safe function has an unsafe wrapper

SAFE="src/Sibyl/Safe/TimeSeries.hs"
UNSAFE="src/Sibyl/TimeSeries.hs"

# extract names
safe_fns=$(grep -E '^[a-z][a-zA-Z0-9]* ::' "$SAFE" \
  | awk '{print $1}' \
  | while read -r name; do
      # check for Eithers
      if grep -qE "^$name\s*::.*Either" "$SAFE"; then
        echo "$name"
      fi
    done)

unsafe_fns=$(grep -E '^[a-z][a-zA-Z0-9]* ::' "$UNSAFE" | awk '{print $1}')

missing=()
while IFS= read -r fn; do
  if ! echo "$unsafe_fns" | grep -qx "$fn"; then
    missing+=("$fn")
  fi
done <<< "$safe_fns"

if [ ${#missing[@]} -eq 0 ]; then
  echo "OK: all Either-returning Safe functions have wrappers"
else
  echo "MISSING unsafe counterparts for:"
  for fn in "${missing[@]}"; do
    echo "  - $fn"
  done
  exit 1
fi
