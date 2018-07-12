#!/bin/bash
exec ghci \
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
