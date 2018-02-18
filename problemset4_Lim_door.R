
myFunction<-function(doorthing, doorthing2, x){
  doorthing1<-doorthing2<-sample(1:3, 1)
  if (doorthing1==doorthing2){ x<-TRUE } else { x==FALSE }
  x
}
myFunction(sample(1:3, 1), sample(1:3, 1))
debug(myFunction)
myFunction(sample(1:3, 1), sample(1:3, 1)) #it is a wierd function

#correct code
myFunction<-function(door.car, my.choice){
  door.car<-sample(1:3, 1)
  my.choice<-sample(1:3, 1)
  if (door.car==my.choice){ outcome<-TRUE} 
  else { outcome<-FALSE }
  return(outcome)
}
myFunction(sample(1:3, 1), sample(1:3, 1))

#Moving on
chosenDoor<-sample(1:3,1)
carDoor<-sample(1:3,1)
switch<-c(TRUE,FALSE)

setClass(Class="Door", 
         representation=representation(chosenDoor = "integer", carDoor="integer", switch="logical"),
         prototype=prototype(chosenDoor=integer(), carDoor=integer(), switch=logical()))
new("Door")
new("Door", chosenDoor=1L, carDoor=2L, switch=TRUE)

is.integer(1)
is.integer(1L)


## Now we can make a "test" that an object must pass when the method 'validObject() is called
setValidity("Door", function(object){
test1<-(is.integer(chosenDoor))
test2<-is.integer(carDoor)
test3<-is.logical(switch)
  if(!test1 || !test2 || !test3){return("@Door is not a valid value")}
} )

example <-new("Door", chosenDoor=1L, carDoor=2L, switch=TRUE)

example <-new("Door", chosenDoor=1, carDoor=2, switch=TRUE)

setMethod("initialize", "Door", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})


setGeneric("PlayGame", 
           function(object="Door"){
             standardGeneric("PlayGame")
           })


example <-new("Door", chosenDoor=1L, carDoor=2L, switch=TRUE)
example


setMethod(f="PlayGame",signature="Door",
          def=function(object){
            chosenDoor <- object@chosenDoor
            carDoor <- object@carDoor
            switch <- object@switch
            draw1<-as.integer(sample(1:3,1))
            carDoor<-as.integer(sample(1:3,1))
            
            condition<-c(draw1,carDoor)
            options<-c(1:3)
            condition
            if (isTRUE(switch)==F) {chosenDoor<-draw1} 
            else {deletedoor<-sample(subset(options, !(options %in% condition)),1)
            cannotchoose<-c(draw1,deletedoor)
            possibledoor<-options[!options %in% cannotchoose]
            chosenDoor<-sample(possibledoor,1)}
            if (chosenDoor==carDoor){winner<-TRUE}
            else {winner<-FALSE}
            print(winner)
          })


PlayGame(example)

sim1<-sapply(1:1000, function(i){PlayGame(new("Door", chosenDoor=1L, carDoor=2L, switch=FALSE))})
sim2<-sapply(1:1000, function(i){PlayGame(new("Door", chosenDoor=1L, carDoor=2L, switch=TRUE))})
table(sim1)
sim1<-as.data.frame(table(sim1)) #switch=false
sim1[2,2]/1000 #winning prob
table(sim2)
sim2<-as.data.frame(table(sim2)) #switch=true
sim2[2,2]/1000  #winning prob   

#switch strategy is the best. #answer

