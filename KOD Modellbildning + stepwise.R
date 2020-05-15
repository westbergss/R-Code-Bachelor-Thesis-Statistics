##### Slutgiltig modell 1:

#log + original
s1 <- lm(Vardkostnad ~ varddagar + lannr + MVO + mdc, data=df1)
s2 <- lm(lnVardkostnad ~ varddagar + lannr + MVO + mdc, data=df1)
s3 <- lm(Vardkostnad ~ lnvarddagar + lannr + MVO + mdc, data=df1)
s4 <- lm(lnVardkostnad ~ lnvarddagar + lannr + MVO + mdc, data=df1)

#log + trans
s5 <- lm(lnVardkostnad ~ varddagar + lannr + MVO + mdc + varddagar*lannr, data=df1)
s6 <- lm(lnVardkostnad ~ varddagar + lannr + MVO + mdc + varddagar*MVO, data=df1)
s7 <- lm(lnVardkostnad ~ varddagar + lannr + MVO + mdc + varddagar*mdc, data=df1)
s8 <- lm(Vardkostnad ~ lnvarddagar + lannr + MVO + mdc + lnvarddagar*lannr, data=df1)
s9 <- lm(Vardkostnad ~ lnvarddagar + lannr + MVO + mdc + lnvarddagar*MVO, data=df1)
s10 <- lm(Vardkostnad ~ lnvarddagar + lannr + MVO + mdc + lnvarddagar*mdc, data=df1)
s11 <- lm(lnVardkostnad ~ lnvarddagar + lannr + MVO + mdc + lnvarddagar*lannr, data=df1)
s12 <- lm(lnVardkostnad ~ lnvarddagar + lannr + MVO + mdc + lnvarddagar*MVO, data=df1)
s13 <- lm(lnVardkostnad ~ lnvarddagar + lannr + MVO + mdc + lnvarddagar*mdc, data=df1)

#sqrt
s14 <- lm(sqrt(Vardkostnad) ~ varddagar + lannr + MVO + mdc, data=df1)
s15 <- lm(Vardkostnad ~ sqrt(varddagar) + lannr + MVO + mdc, data=df1)
s16 <- lm(sqrt(Vardkostnad) ~ sqrt(varddagar) + lannr + MVO + mdc, data=df1)

#sqrt + trans
s17 <- lm(sqrt(Vardkostnad) ~ varddagar + lannr + MVO + mdc + varddagar*lannr, data=df1)
s18 <- lm(sqrt(Vardkostnad) ~ varddagar + lannr + MVO + mdc + varddagar*MVO, data=df1)
s19 <- lm(sqrt(Vardkostnad) ~ varddagar + lannr + MVO + mdc + varddagar*mdc, data=df1)
s20 <- lm(Vardkostnad ~ sqrt(varddagar) + lannr + MVO + mdc + sqrt(varddagar)*lannr, data=df1)
s21 <- lm(Vardkostnad ~ sqrt(varddagar) + lannr + MVO + mdc + sqrt(varddagar)*MVO, data=df1)
s22 <- lm(Vardkostnad ~ sqrt(varddagar) + lannr + MVO + mdc + sqrt(varddagar)*mdc, data=df1)
s23 <- lm(sqrt(Vardkostnad) ~ sqrt(varddagar) + lannr + MVO + mdc + sqrt(varddagar)*lannr, data=df1)
s24 <- lm(sqrt(Vardkostnad) ~ sqrt(varddagar) + lannr + MVO + mdc + sqrt(varddagar)*MVO, data=df1)
s25 <- lm(sqrt(Vardkostnad) ~ sqrt(varddagar) + lannr + MVO + mdc + sqrt(varddagar)*mdc, data=df1)

#Kvadrat
s26 <- lm(Vardkostnad*Vardkostnad ~ varddagar + lannr + MVO + mdc, data=df1)
s27 <- lm(Vardkostnad ~ varddagar*varddagar + lannr + MVO + mdc, data=df1)
s28 <- lm(Vardkostnad*Vardkostnad ~ varddagar*varddagar + lannr + MVO + mdc, data=df1)


#Kvadrat + trans
s29 <- lm(Vardkostnad*Vardkostnad ~ varddagar + lannr + MVO + mdc + varddagar*lannr, data=df1)
s30 <- lm(Vardkostnad*Vardkostnad ~ varddagar + lannr + MVO + mdc + varddagar*MVO, data=df1)
s31 <- lm(Vardkostnad*Vardkostnad ~ varddagar + lannr + MVO + mdc + varddagar*mdc, data=df1)
s32 <- lm(Vardkostnad ~ varddagar*varddagar + lannr + MVO + mdc + varddagar*varddagar*lannr, data=df1)
s33 <- lm(Vardkostnad ~ varddagar*varddagar + lannr + MVO + mdc + varddagar*varddagar*MVO, data=df1)
s34 <- lm(Vardkostnad ~ varddagar*varddagar + lannr + MVO + mdc + varddagar*varddagar*mdc, data=df1)
s35 <- lm(Vardkostnad*Vardkostnad ~ varddagar*varddagar + lannr + MVO + mdc + varddagar*varddagar*lannr, data=df1)
s36 <- lm(Vardkostnad*Vardkostnad ~ varddagar*varddagar + lannr + MVO + mdc + varddagar*varddagar*MVO, data=df1)
s37 <- lm(Vardkostnad*Vardkostnad ~ varddagar*varddagar + lannr + MVO + mdc + varddagar*varddagar*mdc, data=df1)




#### Stepwise
step1 <- ols_step_best_subset(s1)
step1

step2 <- ols_step_best_subset(s2)
step2

step3 <- ols_step_best_subset(s3)
step3

step4 <- ols_step_best_subset(s4)
step4

step5 <- ols_step_best_subset(s5)
step5

step6 <- ols_step_best_subset(s6)
step6

step7 <- ols_step_best_subset(s7)
step7

step8 <- ols_step_best_subset(s8)
step8

step9 <- ols_step_best_subset(s9)
step9

step10 <- ols_step_best_subset(s10)
step10

step11 <- ols_step_best_subset(s11)
step11

step12 <- ols_step_best_subset(s12)
step12

step13 <- ols_step_best_subset(s13)
step13

step14 <- ols_step_best_subset(s14)
step14

step15 <- ols_step_best_subset(s15)
step15

step16 <- ols_step_best_subset(s16)
step16

step17 <- ols_step_best_subset(s17)
step17

step18 <- ols_step_best_subset(s18)
step18

step19 <- ols_step_best_subset(s19)
step19

step20 <- ols_step_best_subset(s20)
step20

step21 <- ols_step_best_subset(s21)
step21

step22 <- ols_step_best_subset(s22)
step22

step23 <- ols_step_best_subset(s23)
step23

step24 <- ols_step_best_subset(s24)
step24

step25 <- ols_step_best_subset(s25)
step25

step26 <- ols_step_best_subset(s26)
step26

step27 <- ols_step_best_subset(s27)
step27

step28 <- ols_step_best_subset(s28)
step28

step29 <- ols_step_best_subset(s29)
step29

step30 <- ols_step_best_subset(s30)
step30

step31 <- ols_step_best_subset(s31)
step31

step32 <- ols_step_best_subset(s32)
step32

step33 <- ols_step_best_subset(s33)
step33

step34 <- ols_step_best_subset(s34)
step34

step35 <- ols_step_best_subset(s35)
step35

step36 <- ols_step_best_subset(s36)
step36

step37 <- ols_step_best_subset(s37)
step37