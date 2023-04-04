# metabias-apps

## Structure

- Each Shiny app is stored within a top-level directory (`pubbias/`, `phacking/`) under an `app/` directory.
- The copy for an app's text and tooltips can be edited in the files in its `docs/` directory.
- There is some shared content for all the apps (`setup.R` and `styles.css`). These base files are in the top-level `_shared/` directory. The script `_update_shared.R` copies the files from there to each app's `www/_shared/` directory (because all files need to be within an app's directory for deployment). Each app's `global.R` calls `setup.R`, and each app's `ui.R` includes `styles.css`.
- There is one shared `renv` environment and `renv.lock` file for all the apps. The dependencies on metabias packages need to be to their development versions on GitHub because of bug fixes since their release ([PublicationBias](https://github.com/mathurlabstanford/PublicationBias/commit/a18396ba61ef8befe4c50e9585b618801607725c) and [phacking](https://github.com/mathurlabstanford/phacking/commit/feee4af83d5638a91a8be3386103ed79f58db4c4)).

## Deployment

- Any commit message with the string `deploy pubbias` or `deploy hacking` will trigger the deployment of the pubbias or phacking app, respectively.
- This deployment is done by the GitHub action `deploy-apps.yaml`, which in turn uses the action `deploy-single-app.yaml` to deploy a single app (this is so that the name of the app is parametrized and the workflow for deployment a single app doesn't have to be duplicated for each app.
- The deployment uses the `rsconnect` package to deploy the app to the `qsu-stanford` account on [shinyapps.io](https://www.shinyapps.io/). For a different account to work for this, an account token for it needs to be generated on [shinyapps.io](https://www.shinyapps.io/) and then added to this repository's secrets ([more info here](qsu-stanford)).
- If an app is having performance issues, there are many scaling settings in [shinyapps.io](https://www.shinyapps.io/) that can be reconfigured.
