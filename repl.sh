#!/bin/bash
exec ghci \
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
