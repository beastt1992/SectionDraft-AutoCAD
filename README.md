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
- Draw 2cm plaster lines everywhere
- **Plan changed? Start all over again.**

The old workflow: Drawing endless `XLINE`s, trimming for hours, and constantly cross-referencing the floor plan to see if a wall is solid, if a door has a sill, or if it's an exposed balcony.

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
  - Automatically draws 2cm Plaster Lines perfectly wrapped
  - Automatically detects Roof Setbacks & Parapets

---

## How It Works

SectionDraft doesn't just blindly project lines. It uses an advanced **Semantic Proximity Radar** and **Topological Inference**:

1. **Touch-to-Trigger:** The core ray (blade) must physically touch a door/window block to cut an opening. It completely eliminates "ghost openings" from nearby geometry.
2. **Adaptive Boundary Radar:** When an opening is detected, the script dynamically reads the Bounding Box of the window/door block and shoots targeted rays (±5cm from edges) to capture the exact connecting wall lines, even for extremely long continuous windows.
3. **Topological Setback Inference:** When drawing Floor N, the script checks Floor N+1. If there are no walls above, it automatically knows it's an exposed roof/balcony and caps it with a waterproof slab and parapet.

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
7. Enter floor heights and slab thickness.
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
| **Auto-L10N (Bilingual)** | Automatically detects your AutoCAD `SYSCODEPAGE`. Displays English globally and Traditional/Simplified Chinese for CJK users. No mojibake! |
| **Global Memory** | Remembers selected layers, floor heights, sorting direction, and plaster settings across your AutoCAD session. |
| **Plaster Lines** | Automatically offsets 2cm (customizable) around slabs and walls, perfectly joining at corners. |
| **Voids / Stairs** | Allows manual selection of void boundaries to prevent slabs from passing through stairwells or elevator shafts. |

---

## Compatibility

| Version | Status |
|---------|--------|
| AutoCAD 2014+ | ✅ Supported |

---

## Troubleshooting

**Ghost walls appeared where they shouldn't be?**
Ensure your red cut line isn't accidentally grazing a nearby block or wall. The script uses a strict ±2cm tolerance radar for general walls to prevent ghost lines.

**Missing short walls/sills next to doors?**
If your door frame/sill is drawn on the opening layer using standard lines (`LINE` or `POLYLINE`), SectionDraft will automatically capture it as a solid wall edge using the Adaptive Boundary Radar. Ensure your blocks are well-formed.

**Roof didn't generate properly?**
Make sure the floor plans are aligned consistently, and the cut lines correctly span all floors including the roof plan (RF). The script automatically caps the final selected floor as a roof.

---

## Version History

| Version | Notes |
|---------|-------|
| v90 | **Auto-L10N Engine**: Added automatic bilingual support (EN/ZH) based on system codepage. |
| v89 | **Adaptive Boundary Radar**: Perfected capture of connecting walls for extra-long windows/openings. |
| v88 | **Precision Wall Merge**: Tightened merge gaps to prevent door frames from blending into RC walls. |
| v84 | **Roof Logic Refinement**: Separated roof rendering logic for perfect parapet generation. |

---

## License

MIT License — Free to use, modify, and distribute.

---

**Made with ❤️ for Architectural Drafters & Engineers.**
