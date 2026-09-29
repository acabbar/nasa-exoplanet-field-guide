# Dataset setup

The dashboard uses the **Planetary Systems Composite Parameters** table from the
[NASA Exoplanet Archive](https://exoplanetarchive.ipac.caltech.edu/cgi-bin/TblView/nph-tblView?app=ExoTbls&config=PSCompPars).

The portfolio results were calculated from a snapshot downloaded on **30 July
2026**. That local CSV contains 6,324 confirmed planets and is intentionally not
tracked in Git because it is approximately 81 MB.

## Add the data locally

1. Download the Planetary Systems Composite Parameters table as CSV from the
   NASA Exoplanet Archive.
2. Rename the file to `nasa_exoplanets.csv`.
3. Place it in this directory: `app/data/nasa_exoplanets.csv`.
4. Run the application from the repository root with
   `shiny::runApp("app")`.

The application reads these 22 fields:

```text
pl_name, hostname, disc_year, discoverymethod, disc_facility,
disc_telescope, sy_snum, sy_pnum, pl_orbper, pl_orbsmax, pl_orbeccen,
pl_eqt, pl_insol, pl_dens, pl_rade, pl_bmasse, pl_bmassprov, st_teff,
st_mass, st_rad, st_met, sy_dist
```

NASA updates the archive over time. A fresh download may therefore produce
different totals from the fixed snapshot documented in the project report.
