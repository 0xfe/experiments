#!/usr/bin/env bash
set -euo pipefail

out_path="${ENTITLEMENTS_OUT:-entitlements.plist}"
keychain_group="${KEYCHAIN_GROUP:-com.example.fe}"
app_id_prefix="${APP_ID_PREFIX:-\$(AppIdentifierPrefix)}"

cat > "${out_path}" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>keychain-access-groups</key>
  <array>
    <string>${app_id_prefix}${keychain_group}</string>
  </array>
</dict>
</plist>
EOF

echo "Wrote entitlements to ${out_path}"
