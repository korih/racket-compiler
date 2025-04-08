#!/bin/bash

echo "Running script"

cd ./tests/ || {
  echo "Directory Not Found"
  exit 1
}

for file in *; do
  if [ -f "$file" ]; then
    raco test "$file"
  fi

done
