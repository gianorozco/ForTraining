FC = gfortran
DEBUG = -g

SRC_DIR = src
APP_DIR = app
BUILD_DIR = build2
BIN_DIR = bin

SRC_FILES = $(wildcard $(SRC_DIR)/*.f90)
OBJ_FILES = $(patsubst $(SRC_DIR)/%.f90,  $(BUILD_DIR)/%.o, $(SRC_FILES))
MAIN = $(APP_DIR)/main.f90
TARGET = $(BIN_DIR)/debugging_bin

all: $(TARGET)

$(TARGET): $(OBJ_FILES) $(MAIN) | $(BIN_DIR)
	$(FC) $(DEBUG) -o $@ $(OBJ_FILES) $(MAIN)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(DEBUG) -c $< -o $@

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BUILD_DIR) $(BIN_DIR)

.PHONY: all clean
