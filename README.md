# TimeSpaceAdvancedAge
Data from a study on age-related differences in time-space interferences using 3D and 2D stimuli. Results: Older adults' spatial judgments are more affected by irrelevant temporal info (time-on-space effect) than younger adults, with no significant age difference in space-on-time effect. No correlation with cognitive functioning.


Repository Contents

    Data Files:
        demo.txt: Demographic information of the participants.
        tsi1.txt: Data from the 2D version the of time-space interference tasks.
        tsi2.txt: Data from the 3D version of time-space interference tasks.
        tsi12_nback_res.RData: Results from the n-back cognitive functioning tests.
    Scripts:
        Main_Script.R: Main analysis script, including linear mixed-effects models (lmer).
        tsi1.R: Reads and cleans data from the 2D version.
        tsi2.R: Reads and cleans data from the 3D version.
        2D_3D.R: Script for connection 2D and 3D stimuli tasks.
        tests.R: Contains various statistical tests.
        plots.R: Generates plots for data visualization.
        tsi_irr_time.R: Calculates the space-on-time effect
        tsi_irr_space.R: Calculates the time-on-space effect
    Additional Files:
        README.md: This README file.
