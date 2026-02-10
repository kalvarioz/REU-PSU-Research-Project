# TDA Wildfire Simulation Project (A Topology-Based Framework for Assessing the Statistical Robustness of Multi-Layer Coastal Infrastructures)

The increasing frequency and intensity of climate-related threats, particularly in coastal regions, expose the profound vulnerabilities of modern society's critical infrastructure. The systems providing energy, transportation, and telecommunications are not independent entities but form a complex, interconnected web where a failure in one can trigger a cascade of disruptions across the others. Traditional risk assessment methods, which often analyze these sectors in isolation, are no longer sufficient to capture the systemic vulnerabilities that emerge from these interdependencies. This report introduces a novel methodology for quantifying the resilience of these coupled systems by developing a Topology-based Statistical Robustness (TSR) metric.

This TDA project is an interactive Shiny application designed to analyze the impact of wildfires on electrical power grid infrastructure in the western United States. This tool combines geospatial analysis, network science, and topological data analysis (TDA) to assess grid vulnerability and resilience under wildfire conditions, it has been refactored and updated into a modular structure to improve code organization, maintainability, and readability.
<img width="881" height="844" alt="474158604-39778347-c3cd-436b-afad-bc340d51012b" src="https://github.com/user-attachments/assets/7da759cb-5609-4496-8176-9c73c7c76ae8" />


## System Requirements

### Hardware Requirements
- **RAM**: Minimum 16 GB (32-64 GB recommended for large-scale analysis)
- **Storage**: At least 5 GB of free disk space for data and cache files
- **Processor**: Multi-core processor recommended (application uses parallel processing)
- **Display**: 1920x1080 resolution or higher recommended for optimal visualization

### Operating System
The application runs on any platform that supports R:
- **Windows**: Windows 10 or later
- **macOS**: macOS 10.14 (Mojave) or later
- **Linux**: Ubuntu 18.04+, Debian 10+, or other modern distributions

### Software Requirements
- **R**: Version 4.0.0 or higher (4.3.0+ recommended)
- **RStudio**: Latest version recommended (optional but helpful for development)
- **Web Browser**: Modern browser (Chrome, Firefox, Safari, or Edge) for viewing the Shiny app

## Installation Instructions

### Step 1: Install R

#### Windows
1. Visit [CRAN](https://cran.r-project.org/bin/windows/base/)
2. Download the latest R installer (e.g., `R-4.3.2-win.exe`)
3. Run the installer and follow the installation wizard
4. Accept default settings unless you have specific preferences

#### macOS
1. Visit [CRAN](https://cran.r-project.org/bin/macosx/)
2. Download the appropriate `.pkg` file for your macOS version
3. Open the downloaded file and follow installation instructions
4. You may need to install XQuartz for certain graphics features: https://www.xquartz.org/

#### Linux (Ubuntu/Debian)
```bash
# Update package list
sudo apt update

# Install R
sudo apt install r-base r-base-dev

# Verify installation
R --version
```

For other Linux distributions, see: https://cran.r-project.org/bin/linux/

### Step 2: Install RStudio (Optional but Recommended)

1. Visit [RStudio Download Page](https://posit.co/download/rstudio-desktop/)
2. Download the installer for your operating system
3. Install following the provided instructions
4. Launch RStudio to verify installation

### Step 3: Install Required R Packages

Open R or RStudio and run the following commands to install all required packages:

```r
# Install CRAN packages
install.packages(c(
  "shiny",           # Web application framework
  "leaflet",         # Interactive maps
  "sf",              # Spatial data handling
  "igraph",          # Network analysis
  "dplyr",           # Data manipulation
  "data.table",      # Fast data processing
  "lubridate",       # Date/time handling
  "future.apply",    # Parallel processing
  "maps",            # Map data
  "ggplot2",         # Visualization
  "tidyr",           # Data tidying
  "R6",              # Object-oriented programming
  "shinyjs"          # JavaScript operations in Shiny
))

# Install TDA (Topological Data Analysis) packages
install.packages(c(
  "TDA",             # Topological data analysis
  "ripserr",         # Persistent homology
  "TDAstats"         # TDA statistics
))
```

**Note**: Package installation may take 10-30 minutes depending on your internet connection and system speed.

### Step 4: Download Project Files/Clone Github

1. Download all project files including:
   - `app.R` (main entry point)
   - `server.R` (server logic)
   - `ui.R` (user interface)
   - `global.R` (global configuration)
   - `AttackAndCascade.R` (cascade simulation)
   - `TopologicalDataWorkflowWF.R` (TDA workflow)
   - `modules/` folder (all module files)
   - Data files (bus, branch, generator, wildfire data)

2. Place all files in a single directory (e.g., `programs/`)

### Step 5: Verify Data Files

Ensure you have all required data files in the appropriate directories:
- Power grid data (bus, branch, generator CSV files)
- Wildfire perimeter data (RDS or shapefile format)
- Spatial reference data

**Note**: Some data files may need to be obtained separately due to size or licensing restrictions.

## How to Run the Application

### Method 1: Run from RStudio (Recommended for Development)

1. Open RStudio
2. Set working directory: `Session > Set Working Directory > Choose Directory...`
3. Navigate to your project folder
4. Open `app.R`
5. Click the "Run App" button in the top-right of the editor
6. The app will launch in a new window or your default browser

### Method 2: Run from R Console

```r
# Set working directory to project folder
setwd("path/to/WildfireGridResilience")

# Load and run the app
shiny::runApp("app.R")
```

### Method 3: Run from Command Line

```bash
# Navigate to project directory
cd path/to/WildfireGridResilience

# Launch R and run app
R -e "shiny::runApp('app.R')"
```
---

# Running the Program
The file structure should be as follows: 
Perseus:
REU-PSU-Research-Project/Programs/West_10k_program/Perseus/perseus.exe
Dataset:
REU-PSU-Research-Project/Programs/West_10k_program/databases/
Server, Rendering, UI
REU-PSU-Research-Project/Programs/West_10k_program/modules/
server.R, ui.R, TopologicalWorkflowWF.R, global.R:
REU-PSU-Research-Project/Programs/West_10k_program/


## Set working directory to: 
REU-PSU-Research-Project/Programs/West_10k_program/

## Run app.R


## Important Disclaimers
### Known Limitations and Issues

#### Incomplete Features
- **Data Coverage**: Wildfire data may not cover all years or all geographic regions
- **Real-time Data**: Application uses historical data only; no real-time fire tracking
- **Grid Model**: Power grid model is simplified and may not reflect actual operational constraints
- **Performance**: Large-scale analyses may be slow or memory-intensive

#### Known Issues

1. **Memory Leaks**
   - Long-running sessions may consume increasing memory
   - **Workaround**: Restart the app periodically for extended analysis sessions
     
2. **Parallel Processing Warnings**
   - May see warnings about future/parallel processing on some systems
   - **Impact**: Usually harmless but may affect performance
   - **Workaround**: Check console for specific errors; most can be ignored
     
3. **Map Rendering**
   - Leaflet map may occasionally fail to render properly on first load
   - **Workaround**: Refresh the page or restart the app
     
4. **Large Fire Events**
   - Very large fire perimeters (>100,000 acres) may cause visualization slowdowns
   - **Workaround**: Adjust analysis radius or filter to smaller fire events
     
5. **TDA Computation Time**
   - Topological data analysis can be computationally expensive
   - Large networks may take several minutes to analyze
   - **Workaround**: Be patient; progress is shown in the R console
     
6. **Browser Compatibility**
   - Best performance in Chrome or Firefox
   - Safari may have occasional rendering issues with complex maps
   - **Workaround**: Use Chrome or Firefox for best experience
     
7. **Data File Paths**
   - Application assumes specific directory structure for data files
   - **Workaround**: Verify `global.R` configuration matches your file locations
     
8. **Compound Fire Analysis**
   - Analysis of multiple simultaneous fires is experimental
   - Results may be less stable than single-fire analysis
   - **Workaround**: Use with caution; verify results carefully
     
9. **Error Handling**
   - Some edge cases may produce cryptic error messages
   - **Workaround**: Check R console for detailed error information; restart app if needed
     
10. **Session State**
    - Browser refresh will reset all analysis progress
    - **Workaround**: Use download buttons to save results before refreshing
      
### Data Disclaimer
- Wildfire perimeter data is sourced from publicly available government databases
- Power grid data represents a simplified model and may not reflect actual utility infrastructure
- Analysis results are for research and educational purposes only
- **DO NOT use this application for operational decision-making without proper validation**

### Making Changes

When making changes to the application:

- Identify the right module - Find which module contains the function you need to modify
- Edit the module - Make your changes in the appropriate module file
- Test thoroughly - Ensure your changes don't break functionality in dependent modules
- Update documentation - Add comments explaining your changes

---

### Notes
The original WildfireServer.R file is preserved and fully functional, but i would NOT use it, as its incredibly cluttered with multiple functions and features jammed into one file, making it 4000+ lines.
 - All functionality is maintained that WildfireServer.R had the modular files have, 
 - The modular structure uses identical logic, just reorganized for clarity, but both versions can coexist in the same directory
