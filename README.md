[中文版](README_ZH.md) | **English**

---

# SectionDraft.lsp — AutoCAD Smart Section Automation

**Draw one cut line. Let the algorithm build the rest.**

---

## The Problem

Drawing architectural vertical sections in AutoCAD is a tedious, repetitive, and error-prone process:

- Project lines from the floor plan down
- Offset slab thickness and floor heights
- Manually trim wall intersections
- Figure out where the roof shrinks back (setbacks)
- Draw 2cm plaster lines (粉刷線) everywhere
- Cap machine rooms and handle hanging shafts
- **Plan changed? Start all over again.**

The old workflow: Drawing endless `XLINE`s, trimming for hours, and constantly cross-referencing the floor plan to see if a wall is solid, if a door has a sill, if a shaft hangs in mid-air, or if it's an exposed balcony.

---

## The Solution

**SectionDraft** turns this hours-long process into seconds. You simply draw a red cut line on your floor plans, run `SECDRAFT`, and the script instantly generates a fully drafted, BIM-level vertical section.

- **Master Cut Line (Red Line):** Placed across your 1F, 2F, 3F... RF floor plans.
- **Smart Recognition Engine:**
  - Identifies Solid Walls (RC/Brick)
  - Identifies Parapets & Balconies
  - Identifies Openings/Doors (and dynamically catches their sills)
- **Automated Output:**
  - Generates Slabs & Walls
  - Automatically draws 2cm Plaster Lines (粉刷線) perfectly wrapped
  - Automatically detects Roof Setbacks & Parapets
  - **BIM-Level Shaft Auto-Sealing (Machine Rooms & Hanging Voids)**

---

## How It Works

SectionDraft doesn't just blindly project lines. It uses an advanced **Semantic Proximity Radar** and **BIM Topological Inference**:

1. **Touch-to-Trigger:** The core ray (blade) must physically touch a door/window block to cut an opening. It completely eliminates "ghost openings" from nearby geometry.
2. **Adaptive Boundary Radar:** When an opening is detected, the script dynamically reads the Bounding Box of the window/door block and shoots targeted rays (±5cm from edges) to capture the exact connecting wall lines.
3. **Terrace Smart Setbacks:** When generating a floor slab, the script strictly looks at the *solid walls* below it, explicitly ignoring lower-floor parapets. This ensures slabs do not accidentally project outward and cover open-to-sky balconies.
4. **Dual-Wall Shaft Tracking:** Voids and shafts (like elevators/stairs) are tracked dynamically across floors. The script requires a *dual-wall* confirmation (solid walls on both sides of the void) to penetrate the slab.
5. **Outer-Wall Exclusion (Machine Rooms):** If a shaft reaches the top floor and its boundaries perfectly match the building's outer shell, the algorithm immediately recognizes it as a Machine Room (not a shaft) and automatically caps the roof and floor slabs.

---

## Installation

1. Download `SectionDraft.lsp`
2. In AutoCAD, type `APPLOAD`
3. Load the file
4. Type `SECDRAFT` to run

**Tip:** Add to AutoCAD's Startup Suite (Contents) for automatic loading every session.

---

## Setup

### 1. Prepare Your Cut Lines
Draw a continuous `LINE` across each of your floor plans indicating where you want to cut the section.

### 2. Organize Your Layers (Standard Practice)
Ensure your drawing uses consistent layers (e.g., `A-WALL` for walls, `A-OPEN` for doors/windows, `A-BALC` for parapets). 

---

## Usage

Type `SECDRAFT` and follow the prompts. The tool features **Global Memory**, so you can just press `Enter` to speed through previously used settings!

1. Click the base insertion point for 1F on your screen.
2. Box-select a sample of **Solid Walls**.
3. Box-select a sample of **Parapets/Balconies** (Press Enter to skip if none).
4. Box-select a sample of **Doors/Windows/Openings** (Press Enter to skip if none).
5. Box-select **ALL** your Cut Lines at once.
6. Choose sorting direction (Left-to-Right, Bottom-to-Top, etc.).
7. Set up BIM void options and plaster thickness.
8. Done!

---

## Commands

| Command | Description |
|---------|-------------|
| `SECDRAFT` | Run the main section generation script |

---

## Notes

| Feature | Details |
|------|---------|
| **Global Memory** | Remembers selected layers, floor heights, sorting direction, and plaster settings across your AutoCAD session. |
| **Plaster Lines** | Automatically offsets 2cm (customizable) around slabs and walls, perfectly joining at corners. |
| **Smart Void/Shaft** | Select void boundaries once; the script dynamically auto-seals tops (machine rooms) and bottoms (hanging shafts) based on wall topology. |

---

## Compatibility

| Version | Status |
|---------|--------|
| AutoCAD 2014+ | ✅ Supported |

---

## Version History

| Version | Notes |
|---------|-------|
| v121 | **Outer-Wall Exclusion**: Perfected Machine Room detection. Prevents false slab penetrations when shafts align with outer building shells. |
| v120 | **Dual-Wall Tracking**: Auto-seals hanging shafts and intermediate empty floors by strictly requiring dual-side solid walls to continue a void. |
| v111 | **Terrace Smart Setbacks**: Slabs perfectly respect open-to-sky balconies by completely ignoring lower-floor parapets during boundary calculation. |
| v100 | **1F Solid Base Logic**: Introduced robust controls for grounding the first floor and preventing shafts from cutting into the foundation. |
| v89 | **Adaptive Boundary Radar**: Perfected capture of connecting walls for extra-long windows/openings. |
| v84 | **Roof Logic Refinement**: Separated roof rendering logic for perfect parapet generation. |

---

## License

MIT License — Free to use, modify, and distribute.

---

**Made with ❤️ for Architectural Drafters & Engineers.**
