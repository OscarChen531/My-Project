# Pitcher Advanced Data
Pitching1985_2016 <- Pitching[24985:44967,]
pp=Pitching1985_2016
which(pp$stint>2)
# ds[392:395,]
# dim(ds)

pp2=NULL
# 可以調整這裏為起點來檢視
# i=390
i=0
repeat{
  i=i+1
  # 可以調整這裏為終點來檢視
  # if (i>520) break;
  if (i>dim(pp)[1]) break;
  # 用來印出處理的筆數
  print(i)
  s1=i
  if (i<dim(pp)[1]){
    while (pp$stint[i]<pp$stint[i+1]) {
      i=i+1
    }
  }
  s2=i
  b=pp[s2,]
  b[1,6:17]=apply(pp[s1:s2,6:17],2,sum)
  pp2=rbind(pp2,b)
}


# 原始資料
pp[390:400,]
# 檢測的資料
pp2[1:3,]