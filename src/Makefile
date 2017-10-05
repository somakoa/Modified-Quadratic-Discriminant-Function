CXX 	= g++
CXXFLAGS= -std=c++11 -MMD -MP -Wall -Wextra -Winit-self -Wno-missing-field-initializers -fopenmp
#CXXFLAGS= -std=c++11 -Wall
LDFLAGS	=
LIBS	= #static library path
INCLUDE	= -I../include 

BINDIR	= ../bin
TARGET1	= mqdf-train
TARGET2	= mqdf-predict

SRCDIR	= .
SOURCES	= $(wildcard $(SRCDIR)/*.cpp)	
OBJDIR	= ../obj

OBJECTS	= $(addprefix $(OBJDIR)/, $(notdir $(SOURCES:.cpp=.o)))

DEPENDS   = $(OBJECTS:.o=.d)


all:	$(BINDIR)/$(TARGET1) $(BINDIR)/$(TARGET2)


$(BINDIR)/$(TARGET1): $(OBJDIR)/mqdf-train.o $(OBJDIR)/mqdf.o  ###$(OBJECTS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS) $(INCLUDE) 

$(BINDIR)/$(TARGET2): $(OBJDIR)/mqdf-predict.o $(OBJDIR)/mqdf.o
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS) $(INCLUDE) 

$(OBJDIR)/%.o: $(SRCDIR)/%.cpp
	-mkdir -p $(OBJDIR)
	$(CXX) $(CXXFLAGS) -o $@ -c $< $(INCLUDE)


.PHONY: clean
clean:
	-rm -f $(OBJECTS) $(DEPENDS) $(TARGET)

-include $(DEPENDS)
