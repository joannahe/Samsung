library(dplyr)

subject_test<-read.table(".\\UCI HAR Dataset\\test\\subject_test.txt")
subject_train<-read.table(".\\UCI HAR Dataset\\train\\subject_train.txt")
x_train<-read.table(".\\UCI HAR Dataset\\train\\x_train.txt")
y_train<-read.table(".\\UCI HAR Dataset\\train\\y_train.txt")
x_test<-read.table(".\\UCI HAR Dataset\\test\\x_test.txt")
y_test<-read.table(".\\UCI HAR Dataset\\test\\y_test.txt")
active_label<-read.table(".\\UCI HAR Dataset\\activity_labels.txt")
feature<-read.table(".\\UCI HAR Dataset\\features.txt")




#####combine trainning and test dataset
x<-rbind(x_train,x_test)
y<-rbind(y_train,y_test)


#####apply descriptive name label to columns
names(x)<-feature$V2

####apply a index number to the dataset since merge function will reorder the sequence, 
####use index will help to get the orginal oder back 
seq<-seq(1,nrow(x))

#####apply active type number to each record---------
dataset<-cbind(seq,y,x)

###apply active type description to each record-----------
dataset_label=merge(active_label,dataset,by.x="V1",by.y="V1",all=TRUE)


####apply appropriate name label to activity type column
colnames(dataset_label)[1:2]<-c("ActivityNum","ActivityType")
###sort in orginal order-----------------------
dataset_label<-dataset_label[order(dataset_label$seq),]


####combine training and test subject numberhea------
subject<-rbind(subject_train,subject_test)

####combine subject number into the dataset--
 final_output<-cbind(subject,dataset_label)
###select only the mean and standard deviation column --------------
final<-final_output[,grep("(mean|std)\\(\\)",colnames(final_output))]
final<-cbind(final_output[,1:3],final)

##apply appropriate name label to subject number column---
 colnames(final)[1]<-"SubjectNum"
##exclude activity number from the dataset---------
final<-select(final,-(ActivityNum))


##step 5 group by values by Subject and Activity Type----
output<-final %>% group_by(SubjectNum,ActivityType) %>% summarise_each(funs(mean))
write.table(output,file="output.txt",row.names=FALSE)


