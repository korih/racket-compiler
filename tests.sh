#!/bin/bash

echo "Running script"

cd ./passes/ || {
  echo "Directory Not Found"
  exit 1
}

for file in *; do
  if [ -f "$file" ]; then
    raco test "$file"
  fi

done
