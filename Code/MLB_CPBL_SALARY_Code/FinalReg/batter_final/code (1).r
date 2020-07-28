Batting1985_2016<- Batting[62256:102832,]
ds=Batting1985_2016
which(ds$stint>2)
# ds[392:395,]
# dim(ds)

ds2=NULL
# 可以調整這裏為起點來檢視
# i=390
i=0
repeat{
  i=i+1
  # 可以調整這裏為終點來檢視
  # if (i>520) break;
  if (i>dim(ds)[1]) break;
  # 用來印出處理的筆數
  print(i)
  s1=i
  if (i<dim(ds)[1]){
    while (ds$stint[i]<ds$stint[i+1]) {
          i=i+1
    }
  }
  s2=i
  b=ds[s2,]
  b[1,6:17]=apply(ds[s1:s2,6:17],2,sum)
  ds2=rbind(ds2,b)
}

# 原始資料
ds[390:400,]
# 檢測的資料
ds2[1:3,]


