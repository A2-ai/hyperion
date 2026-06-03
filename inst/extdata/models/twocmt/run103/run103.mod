$PROBLEM Base one-compartment oral absorption model created from pharos see run103_metadata.json for details.

$INPUT ID TIME EVID AMT CMT DV MDV WT SEX

$DATA /data/user-homes/matthews/Packages/hyperion/inst/extdata/data/derived/onecmpt-oral-30ind.csv IGNORE=@

$SUBROUTINES ADVAN4 TRANS4

$PK
; Typical values
TVCL = THETA(1)
TVV2  = THETA(2)
TVKA = THETA(3)
TVV3 = THETA(4)
TVQ = THETA(5)

; Individual parameters
CL = TVCL * EXP(ETA(1))
V2  = TVV2  * EXP(ETA(2))
KA = TVKA * EXP(ETA(3))
V3 = TVV3
Q = TVQ


; NONMEM scaling
S2 = V2

$ERROR
; Proportional + additive error model (matches mrgsolve)
IPRED = F
Y = IPRED * (1 + EPS(1)) + EPS(2)

$THETA
(0, 1.219137)     ;TVCL (L/hr)
(0, 38.17552)    ;TVV (L)
(0, 1.330243)     ;TVKA (1/hr)
(0, 10)           ;TVV2 (L)
(0, 1.2)          ;TVQ (L/hr)

$OMEGA BLOCK(2)
0.122       ;OM1 TVCL :EXP
0.074543    ;OM1,2 TVCL,TVV :EXP
0.124       ;OM2 TVV :EXP
$OMEGA
0.122       ;OM3 TVKA :EXP

$SIGMA
0.0375371    ;SIG1 Proportional error (variance, 20% CV)
0.00527      ;SIG2 Additive error (variance, 0.01 mg/L SD)


$ESTIMATION METHOD=1 INTERACTION MAXEVAL=9999 PRINT=5 MSFO=run103.msf
$COV PRINT=E MATRIX = R

$TABLE ID TIME DV PRED IPRED CWRES NPDE NOAPPEND NOPRINT ONEHEADER FILE=run103.tab
$TABLE ID CL V2 KA V3 Q ETAS(1:LAST) NOAPPEND NOPRINT ONEHEADER FIRSTONLY FILE=run103par.tab
