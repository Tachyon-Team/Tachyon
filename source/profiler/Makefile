TARGET=profiler
LIB_TARGET=lib$(TARGET).so
TEST_TARGET=profiler_test
CC=gcc

all: $(LIB_TARGET)

$(LIB_TARGET):
	$(CC) -shared -o $@ tachyon_*.c

lib: $(LIB_TARGET)
	

$(TEST_TARGET):
	$(CC) -o $@ *.c

test: $(TEST_TARGET)
	
	
run: $(TEST_TARGET)
	./$(TEST_TARGET)

clean:
	rm -f $(LIB_TARGET) $(TEST_TARGET)
