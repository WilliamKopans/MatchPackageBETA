# Match Application

Before launching the application please:


- Download R  ( [Tutorial](https://youtu.be/cX532N_XLIs) )

    
    
- If using a Mac; Install XQuartz ( [Tutorial](https://youtu.be/020jEnon8FA?t=175) )
    
    
    

## There are two ways to install, either with or without Git. Both will work, but I suggest opting for the second method if you do not have experience with Git.


## To Install with Git:
Note: This may take a few minutes (1-5 minutes)

`install.packages("devtools")`

`devtools::install_github("WilliamKopans/MatchPackageBETA",upgrade = c("always"), force = TRUE, quiet = TRUE)`


If you regularly use the vertion control system Git and encounter the error:
> Error: Failed to install 'unknown package' from GitHub:
  HTTP error 401.
  Bad credentials
  
Then run: ``Sys.unsetenv("GITHUB_PAT")``

## To install without Git:

Click [HERE](https://github.com/WilliamKopans/MatchPackageBETA/blob/main/Match_1.0.0.tgz?raw=true) to download the binary package. Then locate the file (usually in your downloads folder) and copy the pathname. To find the pathname easily on a Mac, right click the file and hold option. This should make a button show up saying: `Copy "Match_1.0.0.tgz" as Pathname`

Once you have the pathname coppied, go to R or RStudio. Type: `install.packages("Path to downloaded file")`, putting the pathname within the quotes and click enter.

# Once the package is downloaded:
### To launch the application run:

Match::launchApp()

Note: You will recieve error messages along the lines of "replacing previous import," these indicate that multiple packages share variable names and do not impact the application.
### Finally, click on the local address followed by `Listening on`

# Tutorial
Once application has been opened, click ```Browse``` and navigate to your first data file. This graph will appear on top. Edit the settings below to reflect the data's charecteristics and double check the file on the right hand side. Select the two variables that you want graphed.

On the top bar, click ```Bottom Data Import``` and repeat this process for the second dataset you will work with.

Next, click on ```Graphs``` tab. 

If you want to create a new Tie file, click the ```Download New Tie File``` button which should download an empty tie file into your downloads folder (unless you changed the default). 

Next, click ```File select``` and navigate to the tie file (either the newly created tie file or a preexisting one).

You can now select tie points on the graphs below by clicking on the respective points. There is a field at the top to change the tie point number you are editing and fields to edit the core number below each range slider.

__Important:__ When you are done, select ```Finalize Tie File```. This last step prepares the tie file to be imputed into the C++.
