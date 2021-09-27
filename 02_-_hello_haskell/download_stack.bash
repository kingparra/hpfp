#!/usr/bin/env bash

# Download the latest version of stack, the tarball, signature, and checksum.
wget https://get.haskellstack.org/stable/linux-x86_64.tar.gz{.asc,.sha256}

# Verify the signature
gpg --verify ./*.asc

# Check the file integrity
sha256sum -c ./*.sha256

# Install the stack binary to ~/.local/bin
read -r -p 'Signature and checksum ok? (y/n): '

if [[ "$REPLY" == y ]]; then
	tar -xf linux-x86_64.tar.gz '*/stack' -C ~/.local/bin
else
	printf %s\\n 'Aborting' >&2
	exit 1
fi
