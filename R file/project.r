


    data <- read.csv("insurance.csv",sep=",")
    head(data)

    print("1.Probability that a person is ")
    ugen <- readline(prompt="Male or Female?: ")
    gen<-data[c('GENDER')]
    p1<-sum(gen==ugen)/nrow(data)
    print(p1)


    print("2.Probability that out of 100 persons exactly m are females")
    m <- readline(prompt="m: ")
    x<-dbinom(as.integer(m),100,prob=p1)
    print(x)

    print("3.Probability that out of 100 persons between n to m are females (n<m)")
    n <- readline(prompt="n: ")
    m <- readline(prompt="m: ")
    q<-diff(pbinom(c(as.integer(n),as.integer(m)),100,p1))
    print(q)

    print("4.Find the regression coefficients of relationship between charges and BMI,Age,Children")
    data.lm<-lm(CHARGES~BMI+AGE+CHILDREN+GENDER+REGION+SMOKER,data=data)
    y<-coef(data.lm)
    print(y)

    print("5.Predict the value of charges based on a given set of inputs")
    
    b1 <- as.integer(readline(prompt="BMI 1: "))
    a1 <- as.integer(readline(prompt="AGE 1: "))
    c1 <- readline(prompt="CHILDREN 1: ")
    g1 <- readline(prompt="GENDER 1(male/female): ")
    s1 <- readline(prompt="SMOKER 1(yes/no): ")
    r1 <- readline(prompt="REGION 1(southwest/southest): ")
    
    b2 <- as.integer(readline(prompt="BMI 2: "))
    a2 <- as.integer(readline(prompt="AGE 2: "))
    c2 <- readline(prompt="CHILDREN 2: ")
    g2 <- readline(prompt="GENDER 2(male/female): ")
    s2 <- readline(prompt="SMOKER 2(yes/no): ")
    r2 <- readline(prompt="REGION 2(southwest/southest): ")
    
    b3 <- as.integer(readline(prompt="BMI 3: "))
    a3 <- as.integer(readline(prompt="AGE 3: "))
    c3 <- readline(prompt="CHILDREN 3: ")
    g3 <- readline(prompt="GENDER 3(male/female): ")
    s3 <- readline(prompt="SMOKER 3(yes/no): ")
    r3 <- readline(prompt="REGION 3(southwest/southest): ")
    
    newdata<-data.frame(BMI=c(b1,b2,b3),AGE=c(a1,a2,a3),CHILDREN=c(as.integer(c1),as.integer(c2),as.integer(c3)),GENDER=c(g1,g2,g3),SMOKER=c(s1,s2,s3),REGION=c(r1,r2,r3))
    w<-predict(data.lm,newdata=newdata)
    print(w)

    print("6.Using normal distribution find the probability that a person is less than k years old")
    k <- as.integer(readline(prompt="k: "))
    i<-pnorm(k,mean(data$AGE),sd(data$AGE))
    print(i)

    print("7.Compare sample distribution mean and population mean of age")
    samp<-rep(NA,1000)
    for (b in 1:1000){
        sampmean<-data[sample(nrow(data), 10),]
        samp[b]<-mean(sampmean$AGE)
    }
    sampdist<-mean(samp)
    sampsd<-sd(samp)
    print(sampdist)
    print(mean(data$AGE))

    print("8.Compare sample distribution standard deviation with population")
    print(sampsd*sqrt(10))
    print(sd(data$AGE))

    print("9.Calculate the interval for mean with 95% level of confidence")
    error<-qnorm(0.975)*sampsd/sqrt(10)
    sampdistmean<-sampdist-error
    sampdistmax<-sampdist+error
    print(sampdistmean)
    print(sampdistmax)

    print("10.Given that charges occured is greater than Rs 'r' find the probability that the person is ugend ")
    r <- as.integer(readline(prompt="r: "))
    ugend <-readline(prompt="ugend(male/female): ")
    con<-sum(data$GENDER[data$CHARGES>r]==ugend)/sum(data$CHARGES>r)
    print(con)

    print("11.Given that the number of children is greater than 'child' find the mean charges")
    child <- as.integer(readline(prompt="child: "))
    last<-mean(data$CHARGES[data$CHILDREN>child])
    print(last)

    
