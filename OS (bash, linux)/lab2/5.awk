BEGIN {
    last=0; 
    cnt=0; 
    avg=0;
} 
$2 == last { 
    print $1 ":" $2 ":" $3; 
    avg=avg + $3; 
    cnt=cnt + 1; 
    last=$2
} 
$2 != last {
    print "Average_Running_Children_of_ParentID ="last, "is ", avg/cnt; 
    print $1 ":" $2 ":" $3; 
    last=$2; cnt=1; avg=$3
} 
END {
    avg=avg / cnt; 
    print $1 ":" $2 ":" $3; 
    print "Average_Running_Children_of_ParentID ="last, "is " avg
}