# forestNETN
This package contains functions that import database views from the MIDN/NCBN forest SQL database and or 
CSVs in the MIDN/NCBN forest data package and provides functions to query, summarize, and visualize MIDN/NCBN forest data.
The R package can be installed using `pak::pkg_install('doi-nps/forestNETN')`. Previous archived 
versions of this R package can be found at <a href="www.github.com/katemmiller/forestMIDN">
www.github.com/katemmiller/forestMIDN</a>

This package includes the following functions: 
<ul>
<li>importData: Import views from SQL database, either through local installation (default), or by connecting to the server.</li>           
<li>importCSV: Import views as CSVs from MIDN/NCBN forest data package.</li>             
<li>exportCSV: Export views as CSVs.</li>             
<li>exportNPSForVeg: Export data in a format that works with the NCRN/NPSForVeg R package.</li>       
<li>joinLocEvent: Query plot location and event data.</li>          
<li>joinAdditionalSpecies: Query additional species data.</li> 
<li>joinCWDData: Query CWD volume data.</li>           
<li>joinMicroSaplings: Query sapling data collected in microplots.</li>     
<li>joinMicroShrubData: Query shrub data collected in microplots.</li>    
<li>joinMicroNotes: Query notes recorded in microplots.</li>       
<li>joinQuadData: Query quadrat character data.</li>         
<li>joinQuadSeedlings: Query seedling data collected in quadrats.</li>    
<li>joinQuadSpecies: Query quadrat specis data.</li>       
<li>joinQuadNotes: Query notes recorded in quadrats.</li>         
<li>joinRegenData: Query summarized seedling and sapling data.</li>         
<li>joinSoilLabData: Query soil chemistry data from the lab.</li>      
<li>joinSoilSampleData: Query soil sample data collected in the field.</li>    
<li>joinStandData: Query stand data, including stand height.</li>         
<li>joinStandDisturbance: Query stand disturbance data.</li>  
<li>joinTreeData: Query tree data.</li>          
<li>joinTreeConditions: Query tree condition data.</li>   
<li>joinTreeFoliageCond: Query tree foliage condition data.</li>   
<li>joinTreeVineSpecies: Query vines recorded on trees.</li>  
<li>joinTreeNotes: Query notes recorded for trees.</li>         
<li>joinVisitNotes: Query all notes recorded for each specified visit.</li>        
<li>plotTreeGrowth: Plot tree growth and mortality over time. </li>        
<li>plotTreeMap: Plot tree maps showing tree status, diameter and location within a specified plot.</li>           
<li>prepTaxa: reshapes the plant taxa table for easier joining.</li>             
<li>sumQuadGuilds: summarize quadrat species cover by guilds and native vs exotic.</li>         
<li>sumSapDBHDist: summarize sapling diameter at breast height distributions by 1cm size class.</li>         
<li>sumSpeciesList: generate a list of all species observed on a plot during a given visit.</li>        
<li>sumStrStage: assign structural stage for each plot following the MIDN/NCBN Ecological Integrity Scorecard.</li>          
<li>sumTreeDBHDist: summarize tree diameter at breast height distributions by 10cm size class.</li>        
<li>theme_FVM: custom ggplot2 theme for plotting MIDN/NCBN forest data.</li>  
</ul>

The docs/ folder includes the R Markdown files that generate the forestMIDN tutorial website. 
The index.Rmd generates the index.html, which is the file used by the gitpage for this repo. 


