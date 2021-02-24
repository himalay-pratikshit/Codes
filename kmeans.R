x = c(24.754,20.097,-3.053,1.654,9.362,11.836,6.791,0.174,9.409,11.202,13.569,30.028,33.028,37.486,-3.528)
y = c(-10.609,-8.991,-5.349,-10.369,7.641,17.043,-7.881,-7.107,3.724,-11.377,-15.460,46.388,43.961,40.252,-10.674)
z = c(-13.671,-5.815,-3.410,-8.336,2.057,3.567,-6.791,6.172,-19.997,-9.073,-4.766,-0.493,-5.246,-14.040,-8.293)
kt <-data.frame(x,y,z)
print("3D coordinates")
print(kt)

c1 = c()
c2 = c()
c3 = c()
cI = c()
cF = c()

## Calculating 3 random centroids
random <- kt[sample(1:nrow(kt), 3), ] 
print("Random centroids")
print(random)
d=as.vector(random[1,])
e=as.vector(random[2,])
f=as.vector(random[3,])
## Distance  from the random centroids 
for (i in 1:15){
  d1 = sqrt((random[1,1]-kt[i,1])^2 + (random[1,2]-kt[i,2])^2 + (random[1,3]-kt[i,3])^2)
  c1 = c(c1,d1)
  d2 = sqrt((random[2,1]-kt[i,1])^2 + (random[2,2]-kt[i,2])^2 + (random[2,3]-kt[i,3])^2)
  c2 = c(c2,d2)
  d3 = sqrt((random[3,1]-kt[i,1])^2 + (random[3,2]-kt[i,2])^2 + (random[3,3]-kt[i,3])^2)
  c3 = c(c3,d3)
}
kt <-data.frame(x,y,z,c1,c2,c3)


## Find the minimum in each row for columns c1 to c3

for (i in 1:15){
 j=names(which.min(kt[i,4:6]))
  cI = c(cI,j)
}
kt <-data.frame(x,y,z,c1,c2,c3,cI)
print("c1, c2 and c3 are distances of the atom from the centroids while cI represents the clustering") 
print("1st iteration")
print(kt)


### Step 2
## Finding mean of the coordinates in each cluster to get new centroids
p <-subset(kt, kt$cI == "c1")
q <-subset(kt, kt$cI == "c2")
s <-subset(kt, kt$cI == "c3")
a = c() # coordinates for new c1 centroid
b = c()
c = c()

for (i in 1:3){
a = c(a, mean(p[,i]))
b = c(b, mean(q[,i]))
c = c(c, mean(s[,i]))
}

## Find distances using new set of centroids
c1 = c()
c2 = c()
c3 = c()
kt <- kt[, -c(4:7)]
for (i in 1:15){
  d1 = sqrt((a[1]-kt[i,1])^2 + (a[2]-kt[i,2])^2 + (a[3]-kt[i,3])^2)
  c1 = c(c1,d1)
  d2 = sqrt((b[1]-kt[i,1])^2 + (b[2]-kt[i,2])^2 + (b[3]-kt[i,3])^2)
  c2 = c(c2,d2)
  d3 = sqrt((c[1]-kt[i,1])^2 + (c[2]-kt[i,2])^2 + (c[3]-kt[i,3])^2)
  c3 = c(c3,d3)
}

kt <- data.frame(x,y,z,c1,c2,c3)
#Need to find the minimum in each row for columns c1 to c3

for (i in 1:15){
 
  j=names(which.min(kt[i,4:6]))
  cF = c(cF,j)
}
#replace CI with CF values
kt$cI=kt$cF
kt$cF <- NULL
cF = c()
kt <- data.frame(x,y,z,c1,c2,c3,cI)
#kt

# abc are the final centeroids and def are (n-1)th or the previous centeroids  ..... FURTHER ITERATIONS IN LOOP
while (a[1]!=d[1] & a[2]!=d[2] & a[3]!=d[3] &
       b[1]!=e[1] & b[2]!=e[2] & b[3]!=e[3] &
       c[1]!=f[1] & c[2]!=f[2] & c[3]!=f[3]){
  
  d=a
  e=b
  f=c
  
  
  p <-subset(kt, kt$cI == "c1")
  q <-subset(kt, kt$cI == "c2")
  s <-subset(kt, kt$cI == "c3")
  a = c() # coordinates for new c1 centroid
  b = c()
  c = c()
  
  for (i in 1:3){
    a = c(a, mean(p[,i]))
    b = c(b, mean(q[,i]))
    c = c(c, mean(s[,i]))
  }
 ## Find distances using new set of centroids
    
    kt <- kt[,-c(4:7)]  #Remove old distances
    c1 = c()
    c2 = c()
    c3 = c()
    for (i in 1:15){
      d1 = sqrt((a[1]-kt[i,1])^2 + (a[2]-kt[i,2])^2 + (a[3]-kt[i,3])^2)
      c1 = c(c1,d1)
      d2 = sqrt((b[1]-kt[i,1])^2 + (b[2]-kt[i,2])^2 + (b[3]-kt[i,3])^2)
      c2 = c(c2,d2)
      d3 = sqrt((c[1]-kt[i,1])^2 + (c[2]-kt[i,2])^2 + (c[3]-kt[i,3])^2)
      c3 = c(c3,d3)
    }
    kt <- data.frame(x,y,z,c1,c2,c3)
    #Need to find the minimum in each row for columns c1 to c3
    
    for (i in 1:15){
      
      j=names(which.min(kt[i,4:6]))
      cF = c(cF,j)
    }
    #replace CI with CF values
    kt$cI=kt$cF
    kt$cF <- NULL
    cF = c()
    kt <- data.frame(x,y,z,c1,c2,c3,cI)
  
}

print("Final clustering")
print(kt)
print("Final cluster vector where c1, c2 and c3 are 3 clusters")
print(kt$cI)
print("Centeroid coordinates of the clusters:")
print(a)
print(b)
print(c)

