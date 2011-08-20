COMP            = g++ 
FLAGS           = -g -Wall
LIBS            =  
OBJ_LIST        = i8051.o main.o
TARGET_NAME     = 8051sim

all: $(TARGET_NAME)

$(TARGET_NAME): $(OBJ_LIST)
	$(COMP) $(FLAGS) -o $(TARGET_NAME) $(OBJ_LIST) $(LIBS)

%.o: %.cc %.h
	$(COMP) $(FLAGS) -c $<

clean:
	rm -f *.o *~ core $(TARGET_NAME)
