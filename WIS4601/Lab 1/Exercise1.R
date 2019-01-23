#First, create an empty 3x3 matris
matl<-(matrix(nrow =3, ncol = 3))
matl
#Second, populate this matrix
matl[2,1]=35
matl
matl[1,2]=53
matl

#one example
mat2 = matrix(1:6, nrow = 2, ncol = 3)
matl
#or
x= c(0,1,3,5,19,8)
mat3= matrix(x,nrow = 3,ncol = 2)
matl

id= 1:6
sex=c("m","f","f","m","f","f")
bodymass=c(45,58,60,47,31,57)
# now combine these vectors to create a data frame. 
squirrels=data.frame(Id= id, Sex=sex, Bodymass=bodymass)
squirrels
squirrels$Bodymass
squirrels$Bodymass[squirrels$Sex=="f"]

