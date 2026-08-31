#!/usr/bin/env python3
"""Offline synthetic coverage for coast_report.py (#1947)."""

import contextlib
import io

import coast_report


def tile(x, y, fluid_type=None, terrain_z=20, mat_id=10,
         glacier_zone=False, beyond_glacier=False,
         include_ice=True, ice_surf=None, ice_mode=None):
    result = {
        "x": x,
        "y": y,
        "terrainZ": terrain_z,
        "fluidType": fluid_type,
        "matId": mat_id,
        "glacierZone": glacier_zone,
        "beyondGlacier": beyond_glacier,
    }
    if include_ice:
        result["iceSurf"] = ice_surf
        result["iceMode"] = ice_mode
    return result


def inlet_fixture(kind, seam=False):
    """Build a straight coast plus one requested water shape."""
    world_size = 4
    half_period = world_size * coast_report.CHUNK_SIZE // 2
    tiles = []
    for x in range(-12, 22):
        for y in range(-10, 11):
            fluid_type = None
            terrain_z = 20
            if x < 0:
                fluid_type = "ocean"
                terrain_z = -8
            elif kind == "fjord" and x <= 17 and -1 <= y <= 1:
                fluid_type = "lake"
                terrain_z = -6
            elif kind == "wide_bay" and x <= 8 and -4 <= y <= 4:
                fluid_type = "lake"
                terrain_z = -4
            elif kind == "sealed_lagoon" \
                    and 5 <= x <= 20 and -1 <= y <= 1:
                fluid_type = "lake"
                terrain_z = -6

            mapped_x, mapped_y = x, y
            if seam and x >= 0:
                # The exact tile alias for one world-wide u period:
                # u changes by worldSize*16 while v remains unchanged.
                mapped_x -= half_period
                mapped_y += half_period
            tiles.append(tile(mapped_x, mapped_y, fluid_type, terrain_z))
    return tiles, world_size


def census_for(kind, seam=False):
    tiles, world_size = inlet_fixture(kind, seam)
    topology = coast_report.coastal_topology(tiles, world_size)
    return coast_report.fjord_census(topology, world_size)


def test_fjord_shapes():
    fjords = census_for("fjord")
    assert len(fjords) == 1, fjords
    assert fjords[0]["penetration"] >= coast_report.FJORD_MIN_PENETRATION
    assert fjords[0]["width"] == 3, fjords
    assert census_for("wide_bay") == []
    assert census_for("straight_shore") == []
    assert census_for("sealed_lagoon") == []


def test_wrapped_fjord_is_counted_once():
    fjords = census_for("fjord", seam=True)
    assert len(fjords) == 1, fjords
    assert fjords[0]["width"] == 3, fjords


def test_latitude_bands_and_form_denominators():
    world_size = 8
    assert coast_report.latitude_band((0, 0), world_size) == "temperate"
    assert coast_report.latitude_band((32, 0), world_size) == "high_latitude"
    assert coast_report.latitude_band((45, 0), world_size) == "polar_margin"
    assert coast_report.latitude_band((48, 0), world_size) == "glacier_zone"
    assert coast_report.latitude_band((-48, 0), world_size) == "glacier_zone"

    tiles = []
    for x in range(-8, 24):
        for y in range(-63, 64):
            if x < 0:
                tiles.append(tile(x, y, "ocean", -8))
            else:
                tiles.append(tile(x, y, None, 1))
    metrics = coast_report.legacy_metrics(tiles, world_size=8)
    band_total = sum(
        sum(forms.values()) for forms in metrics["forms_by_band"].values())
    assert band_total == metrics["total"]
    assert sum(
        int(sum(forms.values()) > 0)
        for forms in metrics["forms_by_band"].values()) >= 2


def glacial_fixture(include_ice):
    tiles = [
        tile(-1, 0, "ocean", -5, include_ice=include_ice),
        tile(0, 0, None, 20, include_ice=include_ice),
        tile(-1, 32, "ocean", -5, include_ice=include_ice),
        tile(0, 32, None, 20, include_ice=include_ice),
        tile(31, 13, "ocean", -5, include_ice=include_ice),
        tile(32, 13, None, 20, include_ice=include_ice),
        tile(31, 16, "ocean", -5, include_ice=include_ice,
             ice_surf=1 if include_ice else None,
             ice_mode="basin" if include_ice else None),
        tile(32, 15, "ocean", -5, include_ice=include_ice,
             ice_surf=2 if include_ice else None,
             ice_mode="drape" if include_ice else None),
        tile(32, 16, None, 20, mat_id=coast_report.GLACIER_MAT_ID,
             glacier_zone=True, include_ice=include_ice),
    ]
    return coast_report.glacial_coast_census(
        coast_report.coastal_topology(tiles, world_size=8), world_size=8)


def test_glacial_signal_aggregation():
    census = glacial_fixture(include_ice=True)
    assert census["ice_available"] is True
    assert census["bands"]["temperate"]["total"] == 1
    assert census["bands"]["high_latitude"]["total"] == 1
    assert census["bands"]["polar_margin"]["total"] == 1
    glacier = census["bands"]["glacier_zone"]
    assert glacier["total"] == 1, glacier
    assert glacier["glacier_zone"] == 1, glacier
    assert glacier["glacier_material"] == 1, glacier
    assert glacier["ice"] == 1, glacier
    assert glacier["basin"] == 1, glacier
    assert glacier["drape"] == 1, glacier


def test_missing_ice_is_unavailable():
    census = glacial_fixture(include_ice=False)
    assert census["ice_available"] is False
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        coast_report.print_glacial_coasts(census)
    rendered = out.getvalue()
    assert "ice=unavailable" in rendered, rendered
    assert "basin=unavailable" in rendered, rendered
    assert "drape=unavailable" in rendered, rendered


def main():
    test_fjord_shapes()
    test_wrapped_fjord_is_counted_once()
    test_latitude_bands_and_form_denominators()
    test_glacial_signal_aggregation()
    test_missing_ice_is_unavailable()
    print("coast_report synthetic tests passed")


if __name__ == "__main__":
    main()
