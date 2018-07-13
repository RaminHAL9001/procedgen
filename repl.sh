#!/bin/bash
exec ghci \
     -package-db ./dist/package.conf.inplace \
    -ghci-script .ghci \
    -XDeriveDataTypeable \
    -XDeriveFunctor \
    -XExistentialQuantification \
    -XFlexibleContexts \
    -XFlexibleInstances \
    -XFunctionalDependencies \
    -XGADTs \
    -XGeneralizedNewtypeDeriving \
    -XImplicitParams \
    -XLambdaCase \
    -XMultiParamTypeClasses \
    -XNoMonomorphismRestriction \
    -XOverloadedStrings \
    -XRankNTypes \
    -XScopedTypeVariables \
    -XStandaloneDeriving \
    ;
