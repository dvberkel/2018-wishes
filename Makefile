SOURCE_DIR := src
TARGET_DIR := docs/js
JS_FILES := $(patsubst $(SOURCE_DIR)/%.elm,$(TARGET_DIR)/%.js,$(wildcard $(SOURCE_DIR)/*.elm))

all: $(JS_FILES);

$(TARGET_DIR)/%.js: $(SOURCE_DIR)/%.elm
	elm make $< --output $@
