// Tool types
#define TOOL_CROWBAR "crowbar"
#define TOOL_MULTITOOL "multitool"
#define TOOL_SCREWDRIVER "screwdriver"
#define TOOL_WIRECUTTER "wirecutter"
#define TOOL_WRENCH "wrench"
#define TOOL_WELDER "welder"
#define TOOL_ANALYZER "analyzer"
#define TOOL_MINING "mining"
#define TOOL_SHOVEL "shovel"
#define TOOL_FULTON "fulton"
#define TOOL_PLASMACUTTER "plasmacutter"

/// This cancels the rest of the chain entirely and indicates success.
#define ITEM_INTERACT_SUCCESS (1<<0) // Same as TRUE since most tool (legacy) tool acts return TRUE on success.
/// Return to prevent the rest of the attack chain from being executed / preventing the item user from thwacking the target.
/// Similar to [ITEM_INTERACT_SUCCESS], but does not necessarily indicate success.
#define ITEM_INTERACT_BLOCKING (1<<1)
	/// Only for people who get confused by the naming scheme.
	#define ITEM_INTERACT_FAILURE ITEM_INTERACT_BLOCKING
/// Return to skip the rest of the interaction chain, going straight to attack.
#define ITEM_INTERACT_SKIP_TO_ATTACK (1<<2)
/// Combination flag for any item interaction that blocks the rest of the attack chain
#define ITEM_INTERACT_ANY_BLOCKER (ITEM_INTERACT_SUCCESS | ITEM_INTERACT_BLOCKING)

// If delay between the start and the end of tool operation is less than MIN_TOOL_SOUND_DELAY,
// tool sound is only played when op is started. If not, it's played twice.
#define MIN_TOOL_SOUND_DELAY 20
