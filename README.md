# Match Application

Before launching the application, please:

Note: Both installations are fairly self-explanatory through their websites, but tutorials are added if you run into any questions.

-   Download [R](https://cran.r-project.org/mirrors.html) ( [Tutorial](https://youtu.be/cX532N_XLIs) )

    -   CRAN Mirror location does not matter as long as you choose a location in the USA. All should be equally fast.
    -   The tutorial shows how to install RStudio, but to run the application you do not need RStudio.

-   If using a Mac, Install [XQuartz](https://www.xquartz.org/) ( [Tutorial](https://youtu.be/020jEnon8FA?t=175) ).

-   Download [this file](https://drive.google.com/file/d/1UBYbJjXV85MJSmrg9lHh00LHEZjIvLMK/view?usp=sharing) (top right is download button) and double click the file to open in R or RStudio. Then, run the file. To run the file, select all then press command and return. Exact key bindings may change by operating system. Try control instead of command on a PC.

    -   A lot of packages should start to download and this may take a few minutes. Each package is a matter of kilobytes, so although it looks like a lot, you are downloading the equivalent of one or two smartphone photos.
    -   If you get the message `Do you want to install from sources the package which needs compilation?`, just type `Yes` and click enter/return.

## There are two ways to install the application, either with or without Git. Both will work, but I suggest opting for the second method if you do not have experience with Git.

## To Install with Git:

Note: This may take a few minutes (1-5 minutes)

`install.packages("devtools")`

`devtools::install_github("WilliamKopans/MatchPackageBETA",upgrade = c("always"), force = TRUE, quiet = TRUE)`

If you regularly use the version control system Git and encounter the error: \> Error: Failed to install 'unknown package' from GitHub: HTTP error 401. Bad credentials

Then run: `Sys.unsetenv("GITHUB_PAT")`

## To install without Git:

Click [HERE](https://github.com/WilliamKopans/MatchPackageBETA/blob/main/Match_1.0.1.tgz?raw=true) to download the binary package. Next, locate the file (usually in your downloads folder) and copy the path name. To find the path name easily on a Mac, right-click the file and hold option. This should make a button show up saying: `Copy "Match_1.0.1.tgz" as Pathname`

Once you have the path name copied, go to R or RStudio. In the console window, type: `install.packages("Path to downloaded file",repos = NULL)`, putting the path name within the quotes and click enter. It should look something like this: `install.packages("/Users/williamkopans/Downloads/Match_1.0.1.tgz",repos = NULL)`.

# Once the package is downloaded:

### To launch the application run:

Match::launchApp()

Note: You will receive error messages along the lines of "replacing previous import," these indicate that multiple packages share variable names and do not impact the application. \### Finally, click on the local address followed by `Listening on`

# Tutorial

Once application has been opened, click `Browse` and navigate to your first data file. This graph will appear on top. Edit the settings below to reflect the data's characteristics and double check the file on the right hand side. Select the two variables that you want graphed.

On the top bar, click `Bottom Data Import` and repeat this process for the second data set you will work with.

Next, click on `Graphs` tab.

If you want to create a new Tie file, click the `Download New Tie File` button which should download an empty tie file into your downloads folder (unless you changed the default).

Next, click `File select` and navigate to the tie file (either the newly created tie file or a preexisting one).

You can now select tie points on the graphs below by clicking on the respective points. There is a field at the top to change the tie point number you are editing and fields to edit the core number below each range slider.

**Important:** When you are done, select `Finalize Tie File`. This last step prepares the tie file to be imputed into the C++.
