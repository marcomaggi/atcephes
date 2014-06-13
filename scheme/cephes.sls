(library (cephes (0 1 0))
  (export
    cephes_version_string
    cephes_version_interface_current
    cephes_version_interface_revision
    cephes_version_interface_age
    chbevl
    drand
    polevl
    p1evl
    powi
    sindg
    tandg
    cotdg
    airy
    hyp2f1
    hyperg
    i0
    i0e
    i1
    i1e
    iv
    jv
    k0
    k0e
    k1
    k1e
    kn
    psi
    struve
    bdtr
    bdtrc
    bdtri
    btdtr
    chdtrc
    chdtr
    chdtri
    expx2
    fdtr
    fdtrc
    fdtri
    gamma
    lgam
    gdtr
    gdtrc
    igam
    igamc
    igami
    incbet
    incbi
    kolmogorov
    nbdtr
    nbdtrc
    nbdtri
    ndtr
    erfc
    erf
    ndtri
    pdtr
    pdtrc
    pdtri
    stdtr
    stdtri
    ellie
    ellik
    ellpe
    ellpj
    ellpk
    eclear
    emov
    eabs
    eneg
    eisneg
    eisinf
    eisnan
    einfin
    emovi
    emovo
    ecleaz
    ecleazs
    emovz
    eiisnan
    ecmpm
    eshdn1
    eshup1
    eshdn8
    eshup8
    eshup6
    eshdn6
    eaddm
    esubm
    edivm
    emulm
    emdnorm
    esub
    eadd
    eadd1
    ediv
    emul
    e53toe
    e64toe
    e113toe
    e24toe
    etoe113
    etoe64
    etoe53
    etoe24
    ecmp
    eround
    ltoe
    ultoe
    eifrac
    euifrac
    eshift
    enormlz
    e24toasc
    e53toasc
    e64toasc
    e113toasc
    etoasc
    asctoe24
    asctoe53
    asctoe64
    asctoe113
    asctoe
    asctoeg
    efloor
    efrexp
    eldexp
    eremain
    eiremain
    enan
    esqrt
    eexp
    epow
    epowi
    etodec
    dectoe
    beta
    dawsn
    ei
    expn
    fac
    fresnl
    plancki
    polylog
    revers
    rgamma
    shichi
    sici
    simpsn
    spence
    zeta
    zetac
    polini
    polprt
    polclr
    polmov
    polmul
    poladd
    polsub
    poldiv
    polsbt
    poleva
    polatn
    polsqt
    polsin
    polcos
    polrt)
  (import (rnrs (6)) (cephes compat))
  (define-shared-object "libcephes.so")
  (define-c-function char* cephes_version_string (void))
  (define-c-function signed-int cephes_version_interface_current (void))
  (define-c-function signed-int cephes_version_interface_revision (void))
  (define-c-function signed-int cephes_version_interface_age (void))
  (define-c-function double chbevl (double double* double ))
  (define-c-function int drand (double* ))
  (define-c-function double polevl (double double* int ))
  (define-c-function double p1evl (double double* int ))
  (define-c-function double powi (double int ))
  (define-c-function double sindg (double ))
  (define-c-function double tandg (double ))
  (define-c-function double cotdg (double ))
  (define-c-function int airy (double double* double* double* double* ))
  (define-c-function double hyp2f1 (double double double double ))
  (define-c-function double hyperg (double double double ))
  (define-c-function double i0 (double ))
  (define-c-function double i0e (double ))
  (define-c-function double i1 (double ))
  (define-c-function double i1e (double ))
  (define-c-function double iv (double double ))
  (define-c-function double jv (double double ))
  (define-c-function double k0 (double ))
  (define-c-function double k0e (double ))
  (define-c-function double k1 (double ))
  (define-c-function double k1e (double ))
  (define-c-function double kn (int double ))
  (define-c-function double psi (double ))
  (define-c-function double struve (double double ))
  (define-c-function double bdtr (int int double ))
  (define-c-function double bdtrc (int int double ))
  (define-c-function double bdtri (int int double ))
  (define-c-function double btdtr (double double double ))
  (define-c-function double chdtrc (double double ))
  (define-c-function double chdtr (double double ))
  (define-c-function double chdtri (double double ))
  (define-c-function double expx2 (double int ))
  (define-c-function double fdtr (int int double ))
  (define-c-function double fdtrc (int int double ))
  (define-c-function double fdtri (int int double ))
  (define-c-function double gamma (double ))
  (define-c-function double lgam (double ))
  (define-c-function double gdtr (double double double ))
  (define-c-function double gdtrc (double double double ))
  (define-c-function double igam (double double ))
  (define-c-function double igamc (double double ))
  (define-c-function double igami (double double ))
  (define-c-function double incbet (double double double ))
  (define-c-function double incbi (double double double ))
  (define-c-function double kolmogorov (double ))
  (define-c-function double nbdtr (int int double ))
  (define-c-function double nbdtrc (int int double ))
  (define-c-function double nbdtri (int int double ))
  (define-c-function double ndtr (double ))
  (define-c-function double erfc (double ))
  (define-c-function double erf (double ))
  (define-c-function double ndtri (double ))
  (define-c-function double pdtr (int double ))
  (define-c-function double pdtrc (int double ))
  (define-c-function double pdtri (int double ))
  (define-c-function double stdtr (int double ))
  (define-c-function double stdtri (int double ))
  (define-c-function double ellie (double double ))
  (define-c-function double ellik (double double ))
  (define-c-function double ellpe (double ))
  (define-c-function int ellpj (double double double* double* double* double* ))
  (define-c-function double ellpk (double ))
  (define-c-function void eclear (unsigned-short* ))
  (define-c-function void emov (unsigned-short* unsigned-short* ))
  (define-c-function void eabs (unsigned-short* ))
  (define-c-function void eneg (unsigned-short* ))
  (define-c-function int eisneg (unsigned-short* ))
  (define-c-function int eisinf (unsigned-short* ))
  (define-c-function int eisnan (unsigned-short* ))
  (define-c-function void einfin (unsigned-short* ))
  (define-c-function void emovi (unsigned-short* unsigned-short* ))
  (define-c-function void emovo (unsigned-short* unsigned-short* ))
  (define-c-function void ecleaz (unsigned-short* ))
  (define-c-function void ecleazs (unsigned-short* ))
  (define-c-function void emovz (unsigned-short* unsigned-short* ))
  (define-c-function int eiisnan (unsigned-short* ))
  (define-c-function int ecmpm (unsigned-short* unsigned-short* ))
  (define-c-function void eshdn1 (unsigned-short* ))
  (define-c-function void eshup1 (unsigned-short* ))
  (define-c-function void eshdn8 (unsigned-short* ))
  (define-c-function void eshup8 (unsigned-short* ))
  (define-c-function void eshup6 (unsigned-short* ))
  (define-c-function void eshdn6 (unsigned-short* ))
  (define-c-function void eaddm (unsigned-short* unsigned-short* ))
  (define-c-function void esubm (unsigned-short* unsigned-short* ))
  (define-c-function int edivm (unsigned-short* unsigned-short* ))
  (define-c-function int emulm (unsigned-short* unsigned-short* ))
  (define-c-function void emdnorm (unsigned-short int int long int ))
  (define-c-function void esub (unsigned-short* unsigned-short* unsigned-short* ))
  (define-c-function void eadd (unsigned-short* unsigned-short* unsigned-short* ))
  (define-c-function void eadd1 (unsigned-short* unsigned-short* unsigned-short* ))
  (define-c-function void ediv (unsigned-short* unsigned-short* unsigned-short* ))
  (define-c-function void emul (unsigned-short* unsigned-short* unsigned-short* ))
  (define-c-function void e53toe (unsigned-short* unsigned-short* ))
  (define-c-function void e64toe (unsigned-short* unsigned-short* ))
  (define-c-function void e113toe (unsigned-short* unsigned-short* ))
  (define-c-function void e24toe (unsigned-short* unsigned-short* ))
  (define-c-function void etoe113 (unsigned-short* unsigned-short* ))
  (define-c-function void etoe64 (unsigned-short* unsigned-short* ))
  (define-c-function void etoe53 (unsigned-short* unsigned-short* ))
  (define-c-function void etoe24 (unsigned-short* unsigned-short* ))
  (define-c-function int ecmp (unsigned-short* unsigned-short* ))
  (define-c-function void eround (unsigned-short* unsigned-short* ))
  (define-c-function void ltoe (long* unsigned-short* ))
  (define-c-function void ultoe (unsigned-long* unsigned-short* ))
  (define-c-function void eifrac (unsigned-short* long* unsigned-short* ))
  (define-c-function void euifrac (unsigned-short* unsigned-long* unsigned-short* ))
  (define-c-function int eshift (unsigned-short* int ))
  (define-c-function int enormlz (unsigned-short* ))
  (define-c-function void e24toasc (unsigned-short* char* int ))
  (define-c-function void e53toasc (unsigned-short* char* int ))
  (define-c-function void e64toasc (unsigned-short* char* int ))
  (define-c-function void e113toasc (unsigned-short* char* int ))
  (define-c-function void etoasc (unsigned-short* char* int ))
  (define-c-function void asctoe24 (char* unsigned-short* ))
  (define-c-function void asctoe53 (char* unsigned-short* ))
  (define-c-function void asctoe64 (char* unsigned-short* ))
  (define-c-function void asctoe113 (char* unsigned-short* ))
  (define-c-function void asctoe (char* unsigned-short* ))
  (define-c-function void asctoeg (char* unsigned-short* int ))
  (define-c-function void efloor (unsigned-short* unsigned-short* ))
  (define-c-function void efrexp (unsigned-short* long* unsigned-short* ))
  (define-c-function void eldexp (unsigned-short* long unsigned-short* ))
  (define-c-function void eremain (unsigned-short* unsigned-short* unsigned-short* ))
  (define-c-function void eiremain (unsigned-short* unsigned-short* ))
  (define-c-function void enan (unsigned-short* int ))
  (define-c-function void esqrt (short* short* ))
  (define-c-function void eexp (unsigned-short* unsigned-short* ))
  (define-c-function void epow (unsigned-short* unsigned-short* unsigned-short* ))
  (define-c-function void epowi (unsigned-short* unsigned-short* unsigned-short* ))
  (define-c-function void etodec (unsigned-short* unsigned-short* ))
  (define-c-function void dectoe (unsigned-short* unsigned-short* ))
  (define-c-function double beta (double double ))
  (define-c-function double dawsn (double ))
  (define-c-function double ei (double ))
  (define-c-function double expn (int double ))
  (define-c-function double fac (int ))
  (define-c-function int fresnl (double double* double* ))
  (define-c-function double plancki (double double ))
  (define-c-function double polylog (int double ))
  (define-c-function void revers (double* double* int ))
  (define-c-function double rgamma (double ))
  (define-c-function int shichi (double double* double* ))
  (define-c-function int sici (double double* double* ))
  (define-c-function double simpsn (double* double ))
  (define-c-function double spence (double ))
  (define-c-function double zeta (double double ))
  (define-c-function double zetac (double ))
  (define-c-function void polini (int ))
  (define-c-function void polprt (double* int int ))
  (define-c-function void polclr (double* int ))
  (define-c-function void polmov (double* double* int ))
  (define-c-function void polmul (double* double* double* int int ))
  (define-c-function void poladd (double* double* double* int int ))
  (define-c-function void polsub (double* double* double* int int ))
  (define-c-function int poldiv (double* double* double* int int ))
  (define-c-function void polsbt (double* double* double* int int ))
  (define-c-function double poleva (double* int double ))
  (define-c-function void polatn (double* double* double* int ))
  (define-c-function void polsqt (double* double* int ))
  (define-c-function void polsin (double* double* int ))
  (define-c-function void polcos (double* double* int ))
  (define-c-function int polrt (double* double* int double-complex* ))
  #| end of library |#)

;;; end of file
