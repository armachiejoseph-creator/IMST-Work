# Africa CDC-WHO IMST | User Manual

Welcome to the **Results Framework Matrix** portal. This application is designed to provide a centralized registry for tracking and managing epidemiological indicators and public health activities.

---
  
  ## 📋 1. Getting Started
  To begin, navigate to the **Gallery Registry** tab. This page displays all current indicators stored in the master database.

### Filtering and Searching
- **By Level:** Filter results by *Outcome*, *Output*, or *Long-term Outcome*.
- **By Pillar:** Narrow your view to specific technical areas (e.g., Surveillance, Laboratory, RCCE).
- **Search Bar:** Type keywords to find a specific Indicator ID or Name instantly.

---
  
  ## ✏️ 2. Editing Indicators
  To update information, follow these steps:
  
  1. Click the **Edit Details** button on the bottom of any card in the Gallery.
2. You will be automatically redirected to the **Editor** tab.
3. Modify the necessary fields. If a field is empty in the database, it will appear as a blank space.
4. **History Tracking:** Click the "History" link next to any field label to see a log of previous changes, including who made the change and when.

---
  
  ## 🔐 3. Saving and Audit Logs
  To maintain data integrity, all changes are strictly monitored.

- **Authorized Email:** You must enter your official email address in the "Authorized Email" field before saving.
- **Change Detection:** The system compares your entries against the current database. If no actual changes are detected, the system will not perform a save.
- **Audit Sheet:** Every individual field change is recorded in a separate `audit_log` sheet, preserving the `old_value` and the `new_value`.

---
  
  ## 🔄 4. Data Synchronization
  The application syncs in real-time with Google Sheets. If you believe the data is out of date due to external edits:
  - Click the **Refresh Data** button located in the Filter sidebar of the Gallery.

---
  
  ## 📧 Support
  For technical issues or access requests, please contact the **System Administrator** or the **Modeling Network Secretariat**.