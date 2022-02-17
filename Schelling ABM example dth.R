
#Step 1.
#Make your agents

  number<-2000
  group<-c(rep(0,(51*51)-number),rep(1,number/2),rep(2,number/2))

  #our agents
  table(group)

#Step 2.  
#Make your environment
  neighborhood<-matrix(sample(group,2601,replace=F), ncol=51)
  par(mfrow=c(1,2))
  image(neighborhood,col=c("black","red","green"),axes=F)
  plot(runif(50,0,1),ylab="percent happy",xlab="time",col="white",ylim=c(0,1))

#Step 3.
#define the rules
alike_preference<-0.8

#Step 4.
#Store the outcome
happiness_tracker<-c()


#Create a function to determine the condition of each agent.
#get the coordinates of each neighbor going counter-clockwise
get_neighbors<-function(coords) {
  n<-c()
  for (i in c(1:8)) {
    
    if (i == 1) {
      x<-coords[1] + 1
      y<-coords[2]
    }
    
    if (i == 2) {
      x<-coords[1] + 1
      y<-coords[2] + 1
    }
    
    if (i == 3) {
      x<-coords[1]
      y<-coords[2] + 1
    }
    
    if (i == 4) {
      x<-coords[1] - 1
      y<-coords[2] + 1
    }
    
    if (i == 5) {
      x<-coords[1] - 1
      y<-coords[2]
    }
    
    if (i == 6) {
      x<-coords[1] - 1
      y<-coords[2] - 1
    }
    
    if (i == 7) {
      x<-coords[1]
      y<-coords[2] - 1
    }
    
    if (i == 8) {
      x<-coords[1] + 1
      y<-coords[2] - 1
    }
    
    if (x < 1) {
      x<-51
    }
    if (x > 51) {
      x<-1
    }
    if (y < 1) {
      y<-51
    }
    if (y > 51) {
      y<-1
    }
    n<-rbind(n,c(x,y))
  }
  n
}



#Simulate

#set the number of iterations / time steps
for (t in c(1:50)) {

  ##store vectors of happy and unhappy cells
  happy_cells<-c()
  unhappy_cells<-c()
  
  #Iterate over the 51 * 51 parcels in the neighborhood
  for (j in c(1:51)) {
    for (k in c(1:51)) {
      
      #get the coordinates of the current parcel being evaluated
      current<-c(j,k)
      
      #Assess the values (0,1,2) of the current parcel
      value<-neighborhood[j,k] 
      
      #only evaluate occupied parcels
      if (value > 0) {
        like_neighbors<-0
        all_neighbors<-0
        
        #Use the get_neighbors function to create an array of the coordinates
        #of all of the current parcels neighbors
        neighbors<-get_neighbors(current)
        
        #cycle through each of the 8 neighbors and determine their value
        for (i in c(1:nrow(neighbors))){
          x<-neighbors[i,1]
          y<-neighbors[i,2]
          
          #all_neighbors provides a count of occupied neighbor parcels for a denominator
          if (neighborhood[x,y] > 0) {
            all_neighbors<-all_neighbors + 1
          }
          #like_neighbors provides a count of similar neighbors for the numerator
          if (neighborhood[x,y] == value) {
            like_neighbors<-like_neighbors + 1
          }
        }
        if (is.nan(like_neighbors / all_neighbors)==FALSE) {
          #Calculate whether the composition of the 8 neighbors is below the threshold
          if ((like_neighbors / all_neighbors) < alike_preference) {
            #Store the coordinates of current parcel in a list of unhappy agents if below threshold 
            unhappy_cells<-rbind(unhappy_cells,c(current[1],current[2]))
          }
          else {
            #Store the coordinates of current parcel in a list of happy agents if below threshold 
            happy_cells<-rbind(happy_cells,c(current[1],current[2]))
          }
        }
        
        else {
          happy_cells<-rbind(happy_cells,c(current[1],current[2]))
        }
      }
    }
  }
  
  #Store the fraction of occupied parcels that are happy
  happiness_tracker<-append(happiness_tracker,length(happy_cells)/(length(happy_cells) + length(unhappy_cells)))
  
  #get all the unhappy cells in random order
  rand<-sample(nrow(unhappy_cells))
  for (i in rand) {
    #Select the agent to move and get their value
    mover<-unhappy_cells[i,]
    mover_val<-neighborhood[mover[1],mover[2]]
    
    #select a random parcel to move to
    move_to<-c(sample(1:51,1),sample(1:51,1))
    move_to_val<-neighborhood[move_to[1],move_to[2]]
    
    #If the parcel is occupied select a new parcel to move to
    #Repeat until an unoccupied parcel is selected
    while (move_to_val > 0 ){
      move_to<-c(sample(1:51,1),sample(1:51,1))
      move_to_val<-neighborhood[move_to[1],move_to[2]]
    }
    
    #assign the old parcel to be empty
    neighborhood[mover[1],mover[2]]<-0
    #Assign the new parcel the value of the agent that is moving
    neighborhood[move_to[1],move_to[2]]<-mover_val
  }
  
  
  par(mfrow=c(1,2))
  image(neighborhood,col=c("black","red","green"),axes=F)
  plot(runif(50,0,1),ylab="percent happy",xlab="time",col="white",ylim=c(0,1))
  lines(happiness_tracker,oma = c(0, 0, 2, 0),col="red")
}

