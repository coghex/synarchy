# Local developer gate (#527). `make ci` runs the same checks CI runs
# (.github/workflows/ci.yml) for fast pre-push feedback — see
# tools/ci-local.sh for the details. Kept deliberately simple so it works
# with the GNU make 3.81 that ships on macOS.
.PHONY: ci
ci:
	@bash tools/ci-local.sh
