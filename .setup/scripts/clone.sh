#!/bin/bash

git clone git@github.com:jeksterslab/betaSandwich.git
rm -rf "$PWD.git"
mv betaSandwich/.git "$PWD"
rm -rf betaSandwich
