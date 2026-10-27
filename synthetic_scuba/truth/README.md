# truth/

**THE DATA IN THIS FOLDER IS SIMULATED.** It must not be mistaken for real utility records.

This folder holds quantities known to the generator, not knowledge about real households. Nobody knows the true
savings of a real household, because its counterfactual usage is never observed. The generator knows each synthetic
household's savings only because it planted them, so every effect stored here is a planted assumption. The folder keeps
the name `truth/` so that scripts and readers can find the answer key, but its contents describe the simulated world
only.

| File | Contents |
|---|---|
| `clean_panel.parquet` | Every location-month before the observation layer: structural means, error components, untreated usage, planted effects of each rebate type, potential outcomes `y0` (no turf removal) and `y1` (with turf removal, treated locations only), and the clean realized usage. |
| `billing_clean.parquet` | The clean billing panel (periods, integer meter reads, budgets and bills) before missing rows and errors. |
| `household_savings.csv` | Planted turf savings by treated location, split into structural (stays with the property) and behavioral (redrawn on turnover) parts, plus the planted savings over the first 12 post-months, turnover flags and overlap with other rebate types. |
| `planted_behavioral_by_tenure.csv` | The planted behavioral savings effect for each occupant after the rebate. |
| `planted_other_rebate_effects.csv` | Install months and planted total savings of clothes washer and irrigation controller rebates. |
| `latent_attitude.csv` | Occupant attributes, including the latent conservation attitude that drives selection. |
| `occupancy_events.csv` | Turnover, new construction and closure events. |
| `climate_monthly.parquet` | Monthly ET0, precipitation, effective rainfall and applied irrigation depth by retailer. |
| `observation_flags.parquet` | Which billing rows the observation layer removed or corrupted. |
| `generator_parameters.json` | All generator parameters and realized calibrated values (selection and savings intercepts). |
